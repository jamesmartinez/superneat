
module Main where

import Control.Monad (msum)
import Happstack.Server ( nullConf
                        , simpleHTTP
                        , toResponse
                        , ok
                        , dir 
                        , nullDir
                        , serveDirectory
                        , Browsing (..)
                        )
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H

main = simpleHTTP nullConf $ msum
         [ dir "static" $ serveDirectory DisableBrowsing [] "static"
         , dir "upload" $ ok . toResponse $ upload 
         , nullDir >> (ok . toResponse $ index)] 



upload = template "test" $ H.toHtml ("Hi, this is the uploader")

index = template "test" $ H.toHtml ("Hello, welcome to our page")

template title body =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
        H.body $ body 

