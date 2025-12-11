{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Intro (intro) where

import Control.Monad.Cont (Cont)
import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS (RWST(runRWST),MonadReader(local),MonadTrans(lift))
import qualified Data.Text as T
import FRP.Yampa (constant,time,(<<<),(>>>),Arrow(arr),SF)
import FRP.Yampa.Event (Event)
import Data.Point2 (Point2(Point2))
import Data.Vector2 (vector2)
import Data.AffineSpace (AffineSpace((.+^),origin))

import Activity (arrowWait,fadeFrom,fadeTo,momentary,over,slide,slideStretch,stdLecture,stdWait)
import Controls (Controls)
import ControlsMaps (menuControl)
import Field (screenCenterOffset)
import LabelName (LabelName(Steve,Jeff,Larry,Mark,Sergey))
import Lightarrow (embedArr,Embedding(Embedding),Swont)
import Menu (backgroundMenu,choiceOverlay,columnMenu)
import MusicName (MusicName(CenterTheme,TitleTheme))
import OfflineData ((>.=),OfflineData,OfflineIO)
import Output (clearScreen,drawSprite,drawText,fadeOutMusic,playSoloSound,playSound,restartRepeatingMusic,sentence,Color(Black,White))
import ProseName (ProseName (ButPinchYourselfBitchYoure, IWokeUpAndIts,WhereAmI,AmIDreaming,WelcomeToTheWorldOf,MyNameIsWokeI,InThisWorldThereAre,AndThenThereArePeople,PeopleUsePeopleForFights,LookAtYouYouLook,IKnowIveHeardThat,DoYouRememberYourName))
import SoundName (SoundName(Ignoloof,Drop,Crash,Accept))
import SpriteName (SpriteName(AvatarFront,Woke,Logo,IgnoloofFront,AvatarPortrait))
import StateClass (TextSource(prose,label))
import Message (Message)

intro :: OfflineData -> Swont Controls OfflineIO T.Text 
intro od = (\(x, _, ()) -> x) <$> runRWST k (Embedding id) od
  where
    k = do
        titleDrop
        titleShake
        titleScreen
        instructionScreen
        local (const (embedArr (clearScreen Black >.=))) wakeup
        local (const (embedArr (drawSprite Woke (Point2 54 34) >.=))) welcome
        explanation
        narration <- local (const (embedArr (drawSprite AvatarPortrait (Point2 54 34) >.=))) lookAtYou
        over 0.25 $ arr (>.= narration) <<< slide (Point2 54 34) (Point2 80 34) 0.125 AvatarPortrait
        name <- local (const (embedArr ((narration >.= drawSprite AvatarPortrait (Point2 80 34)) >.=))) pickName
        over 0.25 $ arr (>.= narration) <<< slide (Point2 80 34) (Point2 54 34) 0.125 AvatarPortrait
        narration <- local (const (embedArr (drawSprite AvatarPortrait (Point2 54 34) >.=))) $ letsGo (label name od)
        local (const (Embedding id)) $ enter narration
        lift $ return (label name od)

titleDrop :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
                  ()
                  OfflineData
                  (Cont (SF Controls OfflineIO))
                  ()
titleDrop = do  momentary $ playSound Drop
                over 1.0 $ time >>> arr drp >>> arr draw
    where  drp t  = Point2 20 (98 * t * t - 66)
           draw    = drawSprite Logo --drawText "PEOPLEMON"

titleShake :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
                   ()
                   OfflineData
                   (Cont (SF Controls OfflineIO))
                   ()
titleShake = do  momentary $ draw (shake 0) >.= play
                 over 0.25 $ time >>> arr shake >>> arr draw
    where  shake t  = Point2 20 (32 + 3 * 1 / (1 + 8 * t) * sin (32 * pi * t))
           draw     = drawSprite Logo --drawText "PEOPLEMON"
           play     = playSound Crash >.= restartRepeatingMusic TitleTheme

titleScreen :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
                    ()
                    OfflineData
                    (Cont (SF Controls OfflineIO))
                    ()
titleScreen = do  stdWait initialDraw 
                  momentary $ initialDraw >.= playSound SoundName.Accept
                  over 0.5 $ time >>> arr raise >>> arr drawTitle
  where  raise t      = Point2 20 (32 - 64 * t)
         initialDraw  = drawTitle (Point2 20 32) >.= drawCreator
         drawTitle    = drawSprite Logo --drawText "PEOPLEMON"
         drawCreator  = drawText "by" (Point2 72 80)               >.=
                        drawText "Alex Stuart" (Point2 40 96)      >.=
                        drawText "PRESS Z TO BEGIN" (Point2 16 124)

instructionScreen :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
                          ()
                          OfflineData
                          (Cont (SF Controls OfflineIO))
                          ()
instructionScreen = do  stdWait out
                        momentary $ out >.= playSound SoundName.Accept
                        local (const (embedArr (out >.=))) $ fadeTo Black 1.0
                        momentary $ clearScreen Black >.= fadeOutMusic
  where out =  drawSprite Logo (Point2 20 0)              >.=
               --drawText "PEOPLEMON" (Point2 48 16)        >.=
               drawText "- control keys -" (Point2 16 48)  >.=
               drawText "Return:  Open menu" (Point2 8 64) >.=
               drawText "Arrows:  Move" (Point2 8 76)      >.=
               drawText "Z:       Confirm" (Point2 8 88)   >.=
               drawText "X:       Cancel" (Point2 8 100)   >.=
               drawText "PRESS Z (AGAIN!)" (Point2 16 124)

wakeup :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
               ()
               OfflineData
               (Cont (SF Controls OfflineIO))
               ()
wakeup = do
    n1 <- stdLecture (prose IWokeUpAndIts)
    arrowWait n1
    n2 <- stdLecture (prose WhereAmI)
    arrowWait n2
    n3 <- stdLecture (prose AmIDreaming)
    arrowWait n3

welcome :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
                ()
                OfflineData
                (Cont (SF Controls OfflineIO))
                ()
welcome = do
    fadeFrom Black 0.5
    momentary $ restartRepeatingMusic CenterTheme
    n1 <- stdLecture (prose WelcomeToTheWorldOf)
    arrowWait n1
    n2 <- stdLecture (prose MyNameIsWokeI)
    arrowWait n2
    fadeTo White 0.5 

explanation :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
               ()
               OfflineData
               (Cont (SF Controls OfflineIO))
               ()
explanation = do
    n1 <- stdLecture (prose InThisWorldThereAre)
    arrowWait n1
    n2 <- stdLecture (prose AndThenThereArePeople)
    over 0.25 $ arr (>.= n2) <<< slide (Point2 160 34) (Point2 54 34) 0.25 IgnoloofFront
    local (const (embedArr (drawSprite IgnoloofFront (Point2 54 34) >.=))) $ do
        momentary $ n2 >.= playSoloSound SoundName.Ignoloof
        arrowWait n2
        n3 <- stdLecture (prose PeopleUsePeopleForFights)
        arrowWait n3
        fadeTo White 0.5
 
lookAtYou :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
             ()
             OfflineData
             (Cont (SF Controls OfflineIO))
             OfflineIO 
lookAtYou = do
    fadeFrom White 0.5
    n1 <- stdLecture (prose LookAtYouYouLook)
    arrowWait n1
    n2 <- stdLecture (prose DoYouRememberYourName)
    arrowWait n2
    return n2

pickName :: RWST (Embedding Controls OfflineIO Controls OfflineIO)
            ()
            OfflineData
            (Cont (SF Controls OfflineIO))
            LabelName
pickName = do
    (next, k) <- choiceOverlay (menuControl >>> nameMenu (const pickName))
    next

nameMenu :: Monad m => (() -> m LabelName)
                    -> SF [Message] (OfflineIO,Event(m LabelName,Int))
nameMenu cancel = backgroundMenu 80 96 (Point2 0 0) menu
  where
    menu = columnMenu (map (\n -> (n, return n)) names) (cancel (), 0) 0

names :: [LabelName]
names = [ Jeff, Larry, Mark, Sergey, Steve ]

letsGo :: (MonadTrans t,
           MonadReader
            (Embedding Controls OfflineIO a b)
            (t (Cont (SF a b))),
           MonadState s (t (Cont (SF a b))),
           TextSource s) =>
             T.Text -> t (Cont (SF a b)) OfflineIO
letsGo name = do
    n1 <- stdLecture (\s -> sentence '!' ["Good,", name] `T.append` " " `T.append` prose IKnowIveHeardThat s)
    arrowWait n1
    stdLecture (prose ButPinchYourselfBitchYoure)

enter :: (MonadTrans t,
          MonadReader
            (Embedding a OfflineIO a1 b1)
            (t (Cont (SF a1 b1)))) =>
          OfflineIO -> t (Cont (SF a1 b1)) ()
enter n2 = do
    over 1.0 $ arr (>.= n2) <<< slideStretch 1 0.45 (Point2 54 34) (Point2 60 52) 1.0 AvatarPortrait
    over 0.5 $ constant (drawSprite AvatarFront ((origin .+^ screenCenterOffset) .+^ (vector2 0 (-8))) >.= n2)
    over 0.5 $ constant (clearScreen White)
