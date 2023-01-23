module Model.Parameters.RGB
    ( RGB(..)
    , fromRGB
    , toRGB
    ) where

import Control.Lens ((^.))
import Data.Aeson
import Monomer
import qualified Monomer.Lens as L

data RGB = RGB Int Int Int

instance FromJSON RGB where
    parseJSON = withObject "RGB" $ \v -> RGB
        <$> v .: "r"
        <*> v .: "g"
        <*> v .: "b"

instance ToJSON RGB where
    toJSON (RGB r g b) = object
        [ "r" .= r
        , "g" .= g
        , "b" .= b
        ]

fromRGB :: RGB -> Color
fromRGB (RGB r g b) = rgb r g b

toRGB :: Color -> RGB
toRGB color = RGB r g b where
    r = color ^. L.r
    g = color ^. L.g
    b = color ^. L.b
