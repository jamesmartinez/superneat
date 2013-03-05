{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import Data.ByteString as B
import qualified Network.HTTP as C
import qualified Network.HTTP.Base as C
import System.FilePath
import Network.URI
import Control.Monad
import Control.Applicative
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
                          look,
                          lookFile,
                          decodeBody,
                          defaultBodyPolicy,
                          
                        )
import Text.Blaze ((!))
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory (copyFile)

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
               H.label "Description"
               H.textarea "" ! A.placeholder "please type a description"
                             ! A.name "description"
                             ! A.rows "3"
                             ! A.cols "50" 
               H.br
               H.label "Category"
               H.select ! A.id "categoryList"
                        ! A.size "3"
                        ! A.name "categories"
                        ! A.multiple "multiple" $ do
                   H.option "Beer" ! A.value "Beer"
                   H.option "Women" ! A.value "Women"
                   H.option "Humor" ! A.value "Humor" 
               H.br
               H.label "URL"
               H.input ! A.type_ "text" ! A.name "image_url" ! A.size "40"
               H.br
               H.label "Upload Image"
               H.input ! A.type_ "file" ! A.name "file_upload" ! A.size "40"
               H.br
               H.input ! A.type_ "submit" ! A.value "Submit"

uploadPost = do
    decodeBody (defaultBodyPolicy "/tmp/" (10*10^8) 1000 1000)
    (p,n,ct) <- lookFile "file_upload"
    liftIO $ print p
    case n of
        "" -> return ()
        _  -> liftIO $ copyFile p ("static/" ++ n)

    u <- look "image_url" 
    case parseURI u of
        Nothing    -> return () -- URL doesn't parse
        Just valid -> do
            mf <- liftIO $ getUrl valid
            liftIO $ (B.writeFile . ("static/" ++) . takeFileName . uriPath $ valid) mf
            
    ok $ toResponse $ template "ok" $ toHtml $
        H.a ! A.href "static/" $ "Gallery"

getUrl u = C.simpleHTTP (C.defaultGETRequest_ u) >>= C.getResponseBody
