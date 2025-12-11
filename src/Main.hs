{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Time.Clock.System (getSystemTime,SystemTime(systemNanoseconds,systemSeconds))
import FRP.Yampa (reactimate)
--import SDL hiding (copy,Stereo) 
import SDL (Point(P),V2(..),V4(V4),EventPayload(..),BlendMode(BlendAlphaBlend),RendererType(AcceleratedVSyncRenderer),LocationMode(AbsoluteLocation),pollEvents,initializeAll,quit,getKeyboardState,windowInitialSize,rendererType,createWindow,defaultWindow,createRenderer,defaultRenderer,rendererScale,($=),rendererDrawBlendMode,setMouseLocationMode,cursorVisible,eventPayload,getMouseButtons,getAbsoluteMouseLocation,rendererDrawColor,clear,present)
import SDL.Input.Keyboard.Codes 
import SDL.Mixer (openAudio,Audio(audioOutput,Audio,audioFrequency,audioFormat),Format(FormatS16_Sys),Output(Stereo))
import System.Exit (exitSuccess)
import System.IO (hFlush,stdout)
import System.Random (getStdGen)
import Foreign.C.Types (CInt,CFloat)

import MainAutomaton (mainAutomaton)
import Controls (controlsDefault,readControls,Controls(ctlDown,ctlUp,ctlRight,ctlLeft,ctlQ))
import OfflineData (loadOfflineData,OfflineData(odRenderer),OfflineIO)

desiredAudioSpec :: Audio
desiredAudioSpec = Audio {
    audioFrequency = 44100,
    audioFormat = FormatS16_Sys,
    audioOutput = Stereo
}

screenScale :: V2 CFloat
screenScale = V2 4 4

windowDimensions :: V2 CInt
windowDimensions = fmap round $ (*) <$> screenScale <*> V2 160 144 

floatSeconds :: SystemTime -> Double 
floatSeconds tS = fromIntegral (systemSeconds tS) + fromIntegral (systemNanoseconds tS) / 1000000000

main :: IO ()
main = do
    initializeAll

    window <- createWindow "Peoplemon" (defaultWindow { windowInitialSize = windowDimensions }){- { windowMode = Fullscreen } -}
    renderer <- createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    rendererScale renderer $= screenScale
    rendererDrawBlendMode renderer $= BlendAlphaBlend
    _ <- setMouseLocationMode AbsoluteLocation
    cursorVisible $= False

    openAudio desiredAudioSpec 1024 

    rgen <- getStdGen

    od <- loadOfflineData renderer

    tS <- getSystemTime
    let seconds = floatSeconds tS
    tRef <- newIORef seconds
    fpsRef <- newIORef (replicate 480 seconds)

    reactimate (return controlsDefault) 
               (input tRef)
               (output fpsRef od)
               (mainAutomaton rgen od)

    quit

commands :: [(Double,Maybe Controls)]
commands =
  [ (2.0, Just (controlsDefault { ctlUp = True })),
    (2.0, Just (controlsDefault { ctlRight = True })),
    (2.0, Just (controlsDefault { ctlLeft = True })),
    (2.0, Just (controlsDefault { ctlDown = True })) ]

input :: IORef Double -> Bool -> IO (Double,Maybe Controls)
input tRef _ = do
    es <- pollEvents
    keyHeld <- getKeyboardState
    buttonHeld <- getMouseButtons
    mouse <- getAbsoluteMouseLocation
    seconds <- readIORef tRef
    tS <- getSystemTime
    let seconds'    = floatSeconds tS
        dt          = min 0.016 (seconds' - seconds)
        scaledMouse = flip div <$> P (fmap round screenScale) <*> mouse
        controls    = readControls keyHeld buttonHeld scaledMouse
        dt'                                   -- memory occupancy plummets if
            | keyHeld Scancode0 = 0           -- this feature is disabled
            | keyHeld Scancode1 = 0.25 * dt
            | keyHeld Scancode2 = 4 * dt
            | otherwise         = dt
    when (any (isQuit . eventPayload) es || ctlQ controls) exitSuccess
    writeIORef tRef seconds'
    return (realToFrac dt', Just controls)

isQuit :: EventPayload -> Bool
isQuit QuitEvent              = True
isQuit (WindowClosedEvent _)  = True
isQuit _                      = False

output :: IORef [Double] -> OfflineData -> Bool -> OfflineIO -> IO Bool
output fpsRef od _ action = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    _ <- action od
    present renderer
{-    tS <- getSystemTime
    samples <- readIORef fpsRef
    let samples' = floatSeconds tS : init samples
        diffs = fst $ foldr (\s (difs, prev) -> ((s - prev) : difs, s)) ([], last samples) samples'
        mean = sum diffs / fromIntegral (length diffs)
    putStr $ "\r" ++ show (1 / mean)
    hFlush stdout
    writeIORef fpsRef samples'
-}
    return False 
  where
    renderer = odRenderer od
