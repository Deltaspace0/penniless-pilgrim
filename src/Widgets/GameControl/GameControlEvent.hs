module Widgets.GameControl.GameControlEvent
    ( GameControlEvent(..)
    , handleEvent
    ) where

import Data.Maybe
import Monomer

import Common.Direction
import Widgets.GameControl.ControlledGame
import Widgets.GameControl.GameControlCfg
import Widgets.GameControl.GameControlModel

data GameControlEvent
    = EventSetScore (Maybe Double)
    | EventClick (Int, Int)
    | EventDirection Direction
    | EventStopShake
    deriving (Eq, Show)

type EventHandle sp ep a
    =  (GameControlCfg sp)
    -> (GameControlModel a)
    -> [EventResponse (GameControlModel a) GameControlEvent sp ep]

handleEvent
    :: (ControlledGame a)
    => GameControlCfg sp
    -> WidgetData sp a
    -> EventHandler (GameControlModel a) GameControlEvent sp ep
handleEvent config field _ _ model event = case event of
    EventSetScore score -> setScoreHandle score config model
    EventClick p -> gameHandle (Left p) field config model
    EventDirection d -> gameHandle (Right d) field config model
    EventStopShake -> stopShakeHandle config model

setScoreHandle :: Maybe Double -> EventHandle sp ep a
setScoreHandle score config _ = response where
    response = RequestParent <$> widgetDataSet wdata score
    wdata = fromMaybe (WidgetValue Nothing) wlens
    wlens = WidgetLens <$> _gccNextScore config

gameHandle
    :: (ControlledGame a)
    => Either (Int, Int) Direction
    -> WidgetData sp a
    -> EventHandle sp ep a
gameHandle info field _ model = response where
    response = if null nextGame
        then [Model shakeModel]
        else resetNextScore:rp
    shakeModel = model
        { _gcmShakeNode = Just $ getCurrentPosition game
        }
    rp = RequestParent <$> (widgetDataSet field $ fromJust nextGame)
    nextGame = case info of
        Left p -> moveToPosition p game
        Right d -> moveToDirection d game
    game = fromJust $ _gcmControlledGame model
    resetNextScore = Event $ EventSetScore Nothing

stopShakeHandle :: EventHandle sp ep a
stopShakeHandle _ model = [Model newModel] where
    newModel = model
        { _gcmShakeNode = Nothing
        }
