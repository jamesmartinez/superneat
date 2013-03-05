module Main where

import Control.Monad 
import Happstack.Server

import Upload
import Layout

main = simpleHTTP nullConf $ msum
         [ dir "static" $ serveDirectory EnableBrowsing [] "static"
         , dir "upload" $ method GET  >> (ok . toResponse $ upload) 
         , dir "upload" $ do
             method POST
             uploadPost
         , nullDir >> (ok . toResponse $ front) ] 


