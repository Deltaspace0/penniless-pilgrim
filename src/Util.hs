module Util
    ( showt'
    , requestRenderEvery
    ) where

import Control.Lens
import Data.Text (Text, pack)
import Monomer
import TextShow
import qualified Monomer.Lens as L

showt' :: Double -> Text
showt' number = pack result where
    result = if m == ".0" then i else t
    (i, m) = break (== '.') t
    t = show $ (fromIntegral $ round $ number*1000)/1000

requestRenderEvery
    :: WidgetNode s e
    -> Double
    -> WidgetRequest s e
requestRenderEvery node duration = req where
    req = RenderEvery widgetId period $ Just steps
    widgetId = node ^. L.info . L.widgetId
    period = 10
    steps = fromIntegral $ duration' `div` period
    duration' = floor duration
