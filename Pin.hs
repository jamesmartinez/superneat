{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Types.Pin where
       
import Data.Data (Typeable, Data)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Data.Text (Text)
import qualified Data.Text as T (empty)
import Data.Time.Clock
import Data.IxSet           ( Indexable(..), IxSet, (@=), Proxy(..), getOne
                            , ixFun, ixSet, toDescList, insert, updateIx )
import Control.Monad.Reader (ask)
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Control.Monad.State  (get, put)

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
    , categories  :: [Text]
    , image       :: Text
    , status      :: Status
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Pin)

newtype Category    = Category Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Description = Description Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype User        = User Text        deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Image       = Image Text       deriving (Eq, Ord, Data, Typeable, SafeCopy) 

instance Indexable Pin where
    empty = ixSet [ ixFun $ \p -> [ pinId p ]]


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
newPin pubDate =
    do b@Pins{..} <- get
       let pin = Pin { pinId = nextPinId
                     , description = T.empty
                     , user        = T.empty
                     , date        = pubDate
                     , status      = Unpublished
                     , categories  = []
                     }
       put $ b { nextPinId = succ nextPinId 
               , pins      = insert pin pins
               }
       return pin

updatePin :: Pin -> Update Pins ()
updatePin updatedPin =
    do b@Pins{..} <- get
       put $ b { pins = updateIx (pinId updatedPin) updatedPin pins
               }

pinById :: PinId -> Query Pins (Maybe Pin)
pinById pid =
    do Pins{..} <- ask
       return $ getOne $ pins @= pid

pinsByStatus :: Status -> Query Pins [Pin]
pinsByStatus status =
    do Pins{..} <- ask
       return $ toDescList (Proxy :: Proxy UTCTime) $ pins @= status



$(makeAcidic ''Pins
  [ 'newPin
  , 'updatePin
  , 'pinById
  , 'pinsByStatus
  ])
    
