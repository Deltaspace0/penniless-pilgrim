module Composites.GameControl.GameControlEvent
    ( GameControlEvent(..)
    , handleEvent
    ) where

import Data.Maybe
import Monomer

import Common.Direction
import Composites.GameControl.ControlledGame
import Composites.GameControl.GameControlCfg
import Composites.GameControl.GameControlModel

data GameControlEvent
    = EventSetScore (Maybe Double)
    | EventClick (Int, Int)
    | EventDirection Direction
    | EventStopShake
    | EventPreview (Int, Int)
    | EventResetPreview
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
    EventPreview p -> previewHandle p config model
    EventResetPreview -> resetPreviewHandle config model

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

previewHandle
    :: (ControlledGame a)
    => (Int, Int)
    -> EventHandle sp ep a
previewHandle p _ model = response where
    response =
        [ Model newModel
        , Event $ EventSetScore score
        ]
    newModel = model
        { _gcmPreviewGame = moveToPosition p game
        }
    score = getScoreByPosition p game
    game = fromJust $ _gcmControlledGame model

resetPreviewHandle :: EventHandle sp ep a
resetPreviewHandle _ model = response where
    response =
        [ Model newModel
        , Event $ EventSetScore Nothing
        ]
    newModel = model
        { _gcmPreviewGame = Nothing
        }
