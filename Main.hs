module Main where

import Control.Monad 
import Happstack.Server
import Control.Exception (bracket)
import Data.Acid (openLocalState)
import Data.Acid.Local (createCheckpointAndClose)

import Upload
import Layout
import Pin (noPins)

main = do
    bracket (openLocalState noPins) (createCheckpointAndClose) $
        \acid -> simpleHTTP nullConf $ handlers acid

handlers acid = msum
                [ dir "static" $ serveDirectory EnableBrowsing [] "static"
                , dir "upload" $ method GET  >> (ok . toResponse $ upload) 
                , dir "upload" $ method POST >> uploadPost acid
                , nullDir >> (ok . toResponse $ front) ] 
                
