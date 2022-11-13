module Util
    ( showt'
    ) where

import Data.Text (Text, pack)
import TextShow (showt)

showt' :: Double -> Text
showt' number = pack result where
    result = if m == ".0" then i else t
    (i, m) = break (== '.') t
    t = show $ (fromIntegral $ round $ number*1000)/1000