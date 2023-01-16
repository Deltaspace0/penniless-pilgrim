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

data GameControlRenderer s e a = GameControlRenderer
    { _gcrEnv :: WidgetEnv s e
    , _gcrNode :: WidgetNode s e
    , _gcrRenderer :: Renderer
    , _gcrData :: GameControlData s a
    , _gcrState :: GameControlState
    }

runRenderer :: GameControlRenderer s e a -> IO ()
runRenderer gcRenderer = do
    let wenv = _gcrEnv gcRenderer
        node = _gcrNode gcRenderer
        renderer = _gcrRenderer gcRenderer
        gcData = _gcrData gcRenderer
        state = _gcrState gcRenderer
        config = _gcdConfig gcData
        style = currentStyle wenv node
        vp = getContentArea node style
        ts = wenv ^. L.timestamp
        Rect x y linkSize _ = _gcsFixedRect state
        Rect x' y' linkSize' _ = _gcsOldFixedRect state
        running = _gcsRunning state
        start = _gcsStart state
        delta = fromIntegral $ ts-start
        animationDuration = _gccAnimationDuration config
        progress = max 0 $ min 1 $ delta/animationDuration
    saveContext renderer
    when (running && progress < 1) $ do
        let s = (linkSize'/linkSize)*(1-progress)+progress
            tx = (x'-x)*(1-progress)+(1-s)*(x+(vp ^. L.x))
            ty = (y'-y)*(1-progress)+(1-s)*(y+(vp ^. L.y))
        intersectScissor renderer vp
        setTranslation renderer $ Point tx ty
        setScale renderer $ Point s s
