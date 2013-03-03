{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import Control.Monad
import Control.Monad.Trans (MonadIO(liftIO))
import Happstack.Server ( nullConf,
                          simpleHTTP,
                          toResponse,
                          ok,
                          dir ,
                          method,
                          Method(..),
                          nullDir,
                          serveDirectory,
                          Browsing (..),
                          lookFile,
                          decodeBody,
                          defaultBodyPolicy,
                          
                        )
import Text.Blaze ((!))
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory (renameFile)

main = simpleHTTP nullConf $ msum
         [ dir "static" $ serveDirectory EnableBrowsing [] "static"
         , dir "upload" $ method GET  >> (ok . toResponse $ upload) 
         , dir "upload" $ do
             method POST
             uploadPost
         , nullDir >> (ok . toResponse $ front) ] 



upload = template "Upload" $ uploadForm

front = template "test" $ H.toHtml ("Hello, welcome to our page" :: Text)

shortBody :: Text -> Text -> Html
shortBody t b = template t (toHtml b)

template :: Text -> Html -> Html
template title body =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
        H.body $ body 

uploadForm =
    H.form ! A.enctype "multipart/form-data"
           ! A.method  "POST"
           ! A.action  "/upload" $ do
               H.input ! A.type_ "file" ! A.name "file_upload" ! A.size "40"
               H.input ! A.type_ "submit" ! A.value "Upload"

uploadPost = do
    decodeBody (defaultBodyPolicy "tmp/" (10*10^8) 1000 1000)
    (p,n,ct) <- lookFile "file_upload"
    liftIO $ renameFile p ("static/" ++ n)
    ok $ toResponse $ template "ok" $ toHtml $ do
        H.a ! A.href "static/" $ "Gallery"
        
    
