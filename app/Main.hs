{-# LANGUAGE OverloadedStrings #-}

module Main where

import Monomer

import Model
import UI

main :: IO ()
main = do
    let config =
            [ appWindowState $ MainWindowNormal (1000, 600)
            , appWindowTitle "Penniless Pilgrim"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
        model = initModel $ makeGame 5 5
    startApp model handleEvent buildUI config