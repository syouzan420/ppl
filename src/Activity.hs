{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings, NoMonomorphismRestriction #-}

module Activity where

import Control.Monad.Cont (Cont)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (gets,modify,MonadTrans(lift))
import Control.Monad.State.Class (MonadState)
import qualified Data.Text as T
import FRP.Yampa (Event(NoEvent,Event),SF,Time,constant,identity,iPre,after,edge,now,switch,returnA,(>>>),Arrow((&&&),first,arr))
import Data.Point2 (Point2(Point2))
import Data.Vector2 (vector2)

import ControlsMaps (proseControl)
import Lightarrow (dSwont,swont,timedInterp,timedSequence,Swont,Embedding(Embedding))
import Message (Message(ProseScroll))
import Output (drawRectangle,drawSpriteExplicit,playSoloSound,soundTrigger,Color(White,Translucent),DrawOrientation(Original))
import Ppmn.Parameters (Ppmn(ppmnHitPoints,ppmnMaxHitPoints,ppmnCry),PpmnAction(actionUpdateObject,actionObject))
import qualified TextBox as TB
import SoundName (SoundName(Accept))
import OfflineData ((>.=),OfflineData,OfflineIO)
import Controls (Controls)
import SpriteName (SpriteName)

stdCommentary :: (MonadTrans t,
                  MonadReader
                    (Embedding Controls OfflineIO a b)
                    (t (Cont (SF a b))),
                  MonadState s (t (Cont (SF a b)))) =>
                   (s -> T.Text) -> t (Cont (SF a b)) OfflineIO
stdCommentary = stdProse $ \text -> proseControl >>> TB.writer (TB.autoScrollingProse text)

stdLecture :: (MonadTrans t,
                  MonadReader
                    (Embedding Controls OfflineIO a b)
                    (t (Cont (SF a b))),
                  MonadState s (t (Cont (SF a b)))) =>
                   (s -> T.Text) -> t (Cont (SF a b)) OfflineIO
stdLecture = stdProse (\text -> breathe `switch` const (speak text))
  where
    breathe = constant (TB.drawConstant "", NoEvent) &&& after 0.25 ()
    speak text = proseControl >>> TB.writer (TB.scrollingProse text)

stdProse :: (MonadTrans t,
             MonadReader
               (Embedding Controls OfflineIO a b)
               (t (Cont (SF a b))),
             MonadState s (t (Cont (SF a b)))) =>
            (T.Text -> SF Controls (OfflineIO,Event (OfflineData -> IO a2)))
            -> (s -> T.Text) -> t (Cont (SF a b)) (OfflineData -> IO a2)
stdProse textbox prose = do
    Embedding embed <- ask
    text <- gets prose
    let ls = T.lines text
        first' = init ls
        final = last ls
    mapM_ ((>>= arrowWait) . lift . swont . embed . textbox) first'
    lift . swont $ embed (textbox final)

fadeTo :: (MonadTrans t,
           MonadReader
            (Embedding a OfflineIO a1 b)
            (t (Cont (SF a1 b)))) =>
          Color -> Time -> t (Cont (SF a1 b)) ()
fadeTo color duration = ask >>= \(Embedding embed) -> lift . swont $ embed fade
  where
    fade = timedSequence t (map mask colors ++ [final])
    colors = [Translucent (Translucent color), Translucent color, color, color]
    t = duration / fromIntegral (length colors)
    mask color = constant (drawRectangle 160 144 color (Point2 0 0), NoEvent)
    final = constant (error "fadeTo terminated", Event ())

fadeFrom :: (MonadTrans t,
             MonadReader
              (Embedding a OfflineIO a1 b)
              (t (Cont (SF a1 b)))) =>
            Color -> Time -> t (Cont (SF a1 b)) ()
fadeFrom color duration = ask >>= \(Embedding embed) -> lift . swont $ embed fade
  where
    fade = timedSequence t (map mask colors ++ [final])
    colors = [color, Translucent color, Translucent (Translucent color), Translucent (Translucent (Translucent color))]
    t = duration / fromIntegral (length colors)
    mask color = constant (drawRectangle 160 144 color (Point2 0 0), NoEvent)
    final = constant (error "fadeTo terminated", Event ())

stdHesitation :: (MonadTrans t,
                  MonadReader
                    (Embedding a b2 a1 b1) (t (Cont (SF a1 b1)))
                 ) =>
                 b2 -> t (Cont (SF a1 b1)) ()
stdHesitation output = ask >>= \(Embedding embed) -> lift . swont $ embed (hesitate 0.5 output)

longHesitation :: (MonadTrans t,
                   MonadReader
                     (Embedding a b2 a1 b1) (t (Cont (SF a1 b1)))
                  ) =>
                  b2 -> t (Cont (SF a1 b1)) ()
longHesitation output = ask >>= \(Embedding embed) -> lift . swont $ embed (hesitate 1.5 output)

hesitate :: Time -> c -> SF b (c, Event ())
hesitate t = (&&& after t ()) . constant

stdWait :: (MonadTrans t,
            MonadReader
              (Embedding Controls b2 a1 b1) (t (Cont (SF a1 b1)))
           ) =>
           b2 -> t (Cont (SF a1 b1)) ()
stdWait output = ask >>= \(Embedding embed) -> lift . swont $ embed (wait output)

arrowWait :: (MonadTrans t,
              MonadReader
                (Embedding Controls OfflineIO a1 b1) (t (Cont (SF a1 b1)))
             ) =>
            (OfflineData -> IO a2) -> t (Cont (SF a1 b1)) ()
arrowWait output = ask >>= \(Embedding embed) -> lift . swont $ embed $ (wait output >>> first draw) >>> arrowSound
  where
    draw = (identity &&& (constant 0.5 >>> TB.waitArrow)) >>> arr (uncurry (>.=))

wait :: c -> SF Controls (c,Event ())
wait output = constant output &&& (proseControl 
                            >>> arr (any isProseScroll) >>> edge)

arrowSound :: SF (OfflineData -> IO a2, Event a) (OfflineIO, Event a)
arrowSound = proc (output, trigger) -> do
    sound    <- soundTrigger Accept -< trigger
    trigger' <- iPre NoEvent        -< trigger
    returnA -< (output >.= sound, trigger')

isProseScroll :: Message -> Bool
isProseScroll ProseScroll = True
isProseScroll _           = False

slide :: Point2 Time -> Point2 Time -> Time -> SpriteName -> SF t OfflineIO
slide = slideStretch 1 1

slideStretch :: Time -> Time -> Point2 Time -> Point2 Time -> Time
                     -> SpriteName -> SF t OfflineIO
slideStretch f0 f1 (Point2 x0 y0) (Point2 x1 y1) t1 sprite = proc _ -> do
    f    <- timedInterp f0 f1 t1 -< ()
    x    <- timedInterp x0 x1 t1 -< ()
    y    <- timedInterp y0 y1 t1 -< ()
    returnA -< drawSpriteExplicit Original f White (vector2 0 0) sprite (Point2 x y)

slide' :: Point2 Time -> Point2 Time -> Time -> SF t (Point2 Time)
slide' (Point2 x0 y0) (Point2 x1 y1) t1 = proc _ -> do
    x    <- timedInterp x0 x1 t1 -< ()
    y    <- timedInterp y0 y1 t1 -< ()
    returnA -< Point2 x y

stdHPEffect :: (MonadTrans t,
                MonadReader
                  (Embedding a Time a1 b) (t (Cont (SF a1 b))),
                MonadState s (t (Cont (SF a1 b))),
                PpmnAction s) =>
               Double -> t (Cont (SF a1 b)) Double
stdHPEffect delta = do
    object <- gets actionObject
    Embedding embed <- ask
    let newHP = min maxHP $ max 0 (oldHP + delta)
        oldHP = ppmnHitPoints object
        maxHP = ppmnMaxHitPoints object
    final <- lift . swont $ embed (changeHp oldHP newHP maxHP)
    modify (actionUpdateObject (object { ppmnHitPoints = final }))
    return final

changeHp :: Double -> Double -> Double -> SF b (Time, Event Double)
changeHp oldHP newHP maxHP = changer &&& after duration newHP
  where
    duration = 1.5 * abs (newHP - oldHP) / maxHP
    changer = timedInterp oldHP newHP duration

playCry :: (MonadTrans t,
            MonadReader
              (Embedding a OfflineIO a1 b1) (t (Cont (SF a1 b1)))
           ) =>
           Ppmn -> t (Cont (SF a1 b1)) ()
playCry p = momentary (playSoloSound (ppmnCry p))

momentary :: (MonadTrans t,
              MonadReader
                (Embedding a OfflineIO a1 b1) (t (Swont a1 b1))
             ) =>
             OfflineIO -> t (Swont a1 b1) ()
momentary output = do
    Embedding embed <- ask
    lift . dSwont $ embed $ constant output &&& now ()

over :: (MonadTrans t,
         MonadReader
           (Embedding a OfflineIO a1 b1) (t (Cont (SF a1 b1)))
        ) =>
        Time -> SF a OfflineIO -> t (Swont a1 b1) ()
over interval sf = do
    Embedding embed <- ask
    lift . swont $ embed $ sf &&& (after interval () >>> iPre NoEvent)
