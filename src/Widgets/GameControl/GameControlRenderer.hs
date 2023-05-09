{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControl.GameControlRenderer
    ( GameControlRenderer(..)
    , runRenderer
    ) where

import Control.Lens
import Control.Monad
import Monomer
import qualified Monomer.Lens as L

import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlData
import Widgets.GameControl.GameControlState

data GameControlRenderer s e a b = GameControlRenderer
    { _gcrEnv :: WidgetEnv s e
    , _gcrNode :: WidgetNode s e
    , _gcrRenderer :: Renderer
    , _gcrData :: GameControlData s a b
    , _gcrState :: GameControlState
    }

runRenderer
    :: (GameControlConfig b a)
    => GameControlRenderer s e a b
    -> IO ()
runRenderer GameControlRenderer{..} = do
    let GameControlData{..} = _gcrData
        GameControlState{..} = _gcrState
        vp = getContentArea _gcrNode $ currentStyle _gcrEnv _gcrNode
        Rect x y linkSize _ = _gcsFixedRect
        Rect x' y' linkSize' _ = _gcsOldFixedRect
        delta = fromIntegral $ (_gcrEnv ^. L.timestamp)-_gcsStart
        animation = getAnimationDuration _gcdConfig
        progress = max 0 $ min 1 $ delta/animation
    saveContext _gcrRenderer
    when (_gcsRunning && progress < 1) $ do
        let s = (linkSize'/linkSize)*(1-progress)+progress
            tx = (x'-x)*(1-progress)+(1-s)*(x+(vp ^. L.x))
            ty = (y'-y)*(1-progress)+(1-s)*(y+(vp ^. L.y))
        intersectScissor _gcrRenderer vp
        setTranslation _gcrRenderer $ Point tx ty
        setScale _gcrRenderer $ Point s s
