module Util
    ( showt'
    , requestRenderEvery
    ) where

import Control.Lens
import Data.Text (Text, pack)
import Monomer.Core
import qualified Monomer.Lens as L

showt' :: Double -> Text
showt' number = pack result where
    result = if m == ".0" then i else t
    (i, m) = break (== '.') t
    t = show $ ((fromIntegral r)/1000 :: Double)
    r = round $ number*1000 :: Integer

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
