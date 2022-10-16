{-# LANGUAGE OverloadedStrings #-}

module Main where

import Monomer

import Model hiding (parameters)
import UI

main :: IO ()
main = do
    parameters <- fromFile "./assets/config.json"
    let config =
            [ appWindowState $ MainWindowNormal (1000, 600)
            , appWindowTitle "Penniless Pilgrim"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
        model = initModel_ parameters
    startApp model handleEvent buildUI config