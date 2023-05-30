module Composites.GameControl.GameControlEvent
    ( GameControlEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.Monad
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
    | EventSetReplay Bool
    | EventReplayStep
    deriving (Eq, Show)

type EventHandle sp ep a b
    =  (GameControlCfg sp ep b)
    -> (GameControlModel a b)
    -> [EventResponse (GameControlModel a b) GameControlEvent sp ep]

handleEvent
    :: (ControlledGame a)
    => GameControlCfg sp ep b
    -> WidgetData sp a
    -> EventHandler (GameControlModel a b) GameControlEvent sp ep
handleEvent config field _ _ model event = case event of
    EventSetScore score -> setScoreHandle score config model
    EventClick p -> gameHandle (Left p) field config model
    EventDirection d -> gameHandle (Right d) field config model
    EventStopShake -> stopShakeHandle config model
    EventPreview p -> previewHandle p config model
    EventResetPreview -> resetPreviewHandle config model
    EventSetReplay v -> setReplayHandle v config model
    EventReplayStep -> replayStepHandle config model

setScoreHandle :: Maybe Double -> EventHandle sp ep a b
setScoreHandle score config _ = response where
    response = RequestParent <$> widgetDataSet wdata score
    wdata = fromMaybe (WidgetValue Nothing) wlens
    wlens = WidgetLens <$> _gccNextScore config

gameHandle
    :: (ControlledGame a)
    => Either (Int, Int) Direction
    -> WidgetData sp a
    -> EventHandle sp ep a b
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

stopShakeHandle :: EventHandle sp ep a b
stopShakeHandle _ model = [Model newModel] where
    newModel = model
        { _gcmShakeNode = Nothing
        }

previewHandle
    :: (ControlledGame a)
    => (Int, Int)
    -> EventHandle sp ep a b
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

resetPreviewHandle :: EventHandle sp ep a b
resetPreviewHandle _ model = response where
    response =
        [ Model newModel
        , Event $ EventSetScore Nothing
        ]
    newModel = model
        { _gcmPreviewGame = Nothing
        }

setReplayHandle
    :: (ControlledGame a)
    => Bool
    -> EventHandle sp ep a b
setReplayHandle start _ model = response where
    response = [Model newModel] <> [Event EventReplayStep | start]
    newModel = if start
        then model
            { _gcmControlledGame = startGame
            , _gcmReplaySequence = replaySequence
            }
        else model
            { _gcmControlledGame = _gcmBackupGame model
            , _gcmReplaySequence = []
            }
    startGame = foldM (flip moveToPosition) game path
    replaySequence = tail $ reverse $ (getCurrentPosition game):path
    path = getPreviousPositions game
    game = fromJust $ _gcmControlledGame model

replayStepHandle :: (ControlledGame a) => EventHandle sp ep a b
replayStepHandle config model = response where
    response = if null replaySequence
        then Report <$> _gccOnReplayed config
        else
            [ Model newModel
            , Task $ threadDelay delay >> pure EventReplayStep
            ]
    newModel = model
        { _gcmControlledGame = moveToPosition p game
        , _gcmReplaySequence = tail replaySequence
        }
    p = head replaySequence
    game = fromJust $ _gcmControlledGame model
    replaySequence = _gcmReplaySequence model
    delay = round $ 1000*(getReplayDuration model)
