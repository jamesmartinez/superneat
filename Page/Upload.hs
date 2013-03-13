{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
module Page.Upload where


import Control.Monad.Trans (liftIO, MonadIO)
import Happstack.Server (toResponse, ok, decodeBody, look, lookFile, defaultBodyPolicy, Response, ServerPartT)
import Data.ByteString as B (writeFile)
import qualified Network.HTTP as C
import Network.URI
import System.Directory (copyFile)
import System.FilePath
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Acid as Acid (query, update)
import Data.Time.Clock
import Page.Layout
import Type.Pin
import Type.Superneat
import Control.Lens
import Data.Table

upload = template "Upload" $ uploadForm

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

uploadPost acid = do
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
            liftIO $ getCurrentTime >>= \t -> Acid.update acid $
                NewPin 1 (Description "foo") t [ PinCategory "Women"] Visible
            liftIO $ (B.writeFile . ("static/" ++) . takeFileName . uriPath $ valid) mf
            x <- liftIO $ Acid.query acid AllPins
            liftIO $ print $ (x ^..rows.pinId & over mapped unPinId)
            
    ok $ toResponse $ template "ok" $ toHtml $
        H.a ! A.href "static/" $ "Gallery"

getUrl u = C.simpleHTTP (C.defaultGETRequest_ u) >>= C.getResponseBody
