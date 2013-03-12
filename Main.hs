module Main where

import Control.Monad 
import Happstack.Server
import Control.Exception (bracket)
import Data.Acid
    ( openLocalState
    , AcidState
    , EventState )
import Data.Acid.Local (createCheckpointAndClose)

import Page.Upload
import Page.Layout
import Type.Superneat (Superneat, emptySuperneat, AllPins, NewPin)

main = do
    putStrLn "opened!"
    bracket (openLocalState emptySuperneat) (createCheckpointAndClose) $
        \acid -> simpleHTTP nullConf $ handlers acid

handlers acid = msum
                [ dir "static" $ serveDirectory EnableBrowsing [] "static"
                , dir "upload" $ method GET  >> (ok . toResponse $ upload) 
                , dir "upload" $ method POST >> uploadPost acid
                , nullDir >> (ok . toResponse $ front) ] 
                
