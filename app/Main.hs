{-# LANGUAGE OverloadedStrings #-}

module Main where

import Monomer

import Model
import UI

main :: IO ()
main = do
    model <- initModel $ Just "./assets/config.json"
    let config =
            [ appWindowState $ MainWindowNormal (1000, 600)
            , appWindowTitle "Penniless Pilgrim"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
    startApp model handleEvent buildUI config