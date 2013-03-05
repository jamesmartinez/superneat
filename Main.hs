{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Main where

import Control.Applicative  ((<$>), optional)
import Control.Exception    (bracket)
import Control.Monad        (msum, mzero) 
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (MonadIO(liftIO))
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced   (update', query')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.ByteString as B
import Text.Blaze ((!))
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Data	    (Data, Typeable)
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Text            (Text)
import qualified Data.Text as Text
import Data.Text.Lazy       (toStrict)
import Happstack.Server     ( nullConf,
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
import qualified Network.HTTP as C
import qualified Network.HTTP.Base as C
import Network.URI
import System.Directory (copyFile)
import System.FilePath

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

newtype PinId = PinId { unPinId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
     
data Status =
    Unpublished
  | Published
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Status)
 
data Pin = Pin
    { pinId       :: PinId
    , user        :: Text
    , description :: Text
    , date        :: UTCTime 
    , category    :: Text
    , image       :: Text
    , status      :: Status
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Pin)

newtype Category    = Category Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Description = Description Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype User        = User Text        deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Image       = Image Text       deriving (Eq, ord, Data, Typeable, SafeCopy) 

instance Indexable Pin where
    empty = ixSet [ ixFun $ \bp -> [ pinId bp ]
                  , ixFun $ \bp -> [ Description $ description bp ]
                  , ixFun $ \bp -> [ User $ user bp ]
                  , ixFun $ \bp -> [ status bp ]
                  , ixFun $ \bp -> map Category (categories bp)
                  , ixFun $ (:[]) . date 
                  ] 

ixFun :: (Ord b, Typeable b) => (a -> [b]) -> Ix a

data Pins = Pins
    { nextPinId :: PinId
    , pins      :: IxSet Pin
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Pins)

initialPinsState :: Pins
initialPinsState =
    Pins { nextPinId = PinId 1
         ,  pins = empty
         }

newPin :: UTCTime -> Update Pins Pin
newPin :: pubDate =
    do b@Pins{..} <- get
       let pin = Pin { pinId = nextPinId
                     , description = Text.empty
                     , user        = Text.empty
                     , date        = pubDate
                     , status      = Unpublished
                     , categories  = []
                     }
       put $ b { nextPinId = succ nextPinId 
               , pins      = IxSet.insert pin pins
               }
       return pin

insert :: (Typeable a, Ord a, Indexable a) => a -> IxSet a -> IxSet a

updatePin :: Pin -> Update Pins ()
updatePin updatedPin =
    do b@Pins{..} <- get
       put $ b { pins = IxSet.updateIx (pinId updedPin) updatedPin pins
               }

updateIx :: (Indexable a, Ord a, Typeable a, Typeable key) =>
            key
         -> a
         -> IxSet a
         -> IxSet a

pinById :: PinId -> Query Pins (Maybe Pin)
pinById pid =
    do Pins{..} <- ask
       return $ getOne $ pins @= pid

(@=) :: (Typeable key, Ord a, Typeable a, Indexable a) => IxSet a -> key -> IxSet a

getOne :: Ord a => IxSet a -> Maybe a

pinsByStatus :: Status -> Query Pins [Pin]
pinsByStatus status =
    do Pins{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ pins @= status

toDescList :: (Typeable k, Typeable a, Indexable a) => Proxy k -> IxSet a -> [a]

data Proxy a = Proxy

$(makeAcidic ''Pins
  [ 'newPin
  , 'updatePin
  , 'pinById
  , 'pinsByStatus
  ])
    
