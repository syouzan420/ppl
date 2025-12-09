{-# LANGUAGE Arrows #-}

module Tuner where

import Data.Automaton (Automaton)
import Data.Automaton.Trans.Reader (runReaderS, readerS, ReaderT)
import FRP.Yampa (returnA,integral,SF) 
import Controls (Controls(..))

data Dir = Dir !Bool !Bool !Bool !Bool

transform :: Monad m => Automaton (ReaderT r1 (ReaderT r2 m)) a b
                                -> Automaton (ReaderT r1 m) (r2, a) b
transform automaton = readerS msf
  where
    msf = proc (dt, (x, input)) -> do
        output <- runReaderS (runReaderS automaton) -< (x, (dt, input))
        returnA -< output
instrumentation :: SF ((Double,Double),Controls) (b -> IO a)
                      -> SF Controls (b -> IO ())
instrumentation automaton = proc controls -> do
                            -- up down left right
    (x, drawTuner) <- tuner -< Dir (ctlUp controls) (ctlDown controls) (ctlLeft controls) (ctlRight controls)
    drawGame <- automaton -< (x, controls)
    returnA -< (\r -> drawGame r >> drawTuner r)

tuner :: SF Dir ((Double,Double),b -> IO ())
tuner = proc (Dir up down left right) -> do
    u <- integral -< if up then 100::Double else 0
    d <- integral -< if down then 100::Double else 0
    l <- integral -< if left then 100::Double else 0
    r <- integral -< if right then 100::Double else 0
    let x = -2 + (r - l)
    let y = 183 + (d - u)
    returnA -< ((x, y), const $ print (x, y))
