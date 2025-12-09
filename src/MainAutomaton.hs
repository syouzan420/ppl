{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module MainAutomaton where

import Data.Text.Internal (Text)
import Data.Functor.Identity
import Control.Monad.Cont (runCont,ContT)
import Control.Monad.RWS (RWST(runRWST))
import qualified Data.Map as M
import FRP.Yampa (SF,constant)
import FRP.Yampa.Event (Event)
import Data.Point2 (Point2(Point2))
import System.Random (RandomGen(split),StdGen)

import Controls (Controls)
import Field
import Intro (intro)
import Lightarrow (keep,Embedding(Embedding),KeepSF)
import OfflineData (OfflineData(odGetTerrain),OfflineIO)
import Output (drawText)
import Ppmn.Parameters (PpmnCounters(PpmnCounters))
import Message (Message)

mainAutomaton :: StdGen -> OfflineData -> SF Controls OfflineIO
mainAutomaton rgen od = runCont (intro od >>= field rgen od) final
  where
    final (_, _, ()) = constant $ drawText "FIN" (Point2 100 100)

field :: StdGen -> OfflineData -> Text 
            -> ContT (SF Controls OfflineIO) Identity ((),FieldParameters,()) 
field rgen od name = runRWST k (Embedding id) fp
  where
    k = --callCC (selecting 0)
        --personDetails ignoloofBase
        Field.anchor explore
        --wildBattle (atLevel 3 ignoloofLearnMove ignoloofBase)
        --trainerBattle TheDonald Donald TheGreatestWitchHuntIn [ppmnByName LabelName.Blamotage 1, ppmnByName LabelName.Unner 1]
        --finalBattle
    fp = FieldParameters { 
        fpAvatar = (protagonist (0, 4)) { cRandomGenerator = aRgen },
        fpAvatarName = name,
        fpCounters = PpmnCounters 0 0 0 M.empty,
        fpItems = M.empty, {-
            (Booch, booch { itemStock = 3 }),
            (LabelName.PPhone, pPhone { itemStock = 3 })
            ],-}
        fpLocale = (fpWorld fp) M.! (fpMap fp),
        fpMap = FamilyHouse2F,
        fpOfflineData = od,
        fpPpmn = [],
        fpRandomGenerator = rgen',
        fpWorld = world rgen' od
    }
    (aRgen, rgen') = split rgen

world :: StdGen -> OfflineData 
  -> M.Map MapName (KeepSF 
                      (Character,Event Message) 
                      ((OfflineIO,OfflineIO,Terrain),Event Message)
                   )
world rgen od = M.fromList (zip maps scripts')
  where
    (_, scripts') = foldr setRgen (rgen, []) scripts
    setRgen s (r, ss) = let (r1, r2) = split r in (r2, s r1 : ss)
    scripts = [keep . scriptByMap m (odGetTerrain od m) | m <- maps]
    maps = [toEnum 0 ..]
