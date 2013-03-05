{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}
module Types.Pin where

import Data.Text as Text (Text, empty)
import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.IxSet as IxSet (Indexable, IxSet, empty, ixSet, ixFun, insert, (@=), getOne, updateIx)


newtype UserId = UserId { unUserId :: Integer }
               deriving (Eq, Ord, Num, Typeable, SafeCopy)

newtype UserToken = UserToken Text
                  deriving (Eq, Ord, SafeCopy)

newtype PinId = PinId { unPinId :: Integer }
              deriving (Eq, Ord, Enum, Typeable, SafeCopy)

newtype PinCategory = PinCategory Text
                      deriving (Eq, Ord, Typeable, SafeCopy)

data Visibility   = Visible | Hidden
                  deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''Visibility)

data User = User { userId   :: UserId
                 , token    :: UserToken
                 , nickname :: Text
                 , joined   :: UTCTime }
            deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
  empty = ixSet . map ixFun $ [ \u -> [ userId u ] ]
  
data Pin = Pin { pinId       :: PinId
               , owner       :: UserId
               , description :: Text
               , date        :: UTCTime 
               , categories  :: [PinCategory]
               , visibility  :: Visibility
               }
         deriving (Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''Pin)


instance Indexable Pin where
    empty = ixSet $
             [ ixFun $ \p -> [ pinId      p ]
             , ixFun $ \p -> [ owner      p ]
             , ixFun $ \p -> [ date       p ]
             , ixFun $ \p -> [ categories p ]
             ]

data Pins = Pins { nextPinId :: PinId
                 , pins      :: IxSet Pin }

$(deriveSafeCopy 0 'base ''Pins)

newPin pubDate = do
    ps@Pins{..} <- get
    let pin = Pin { pinId       = nextPinId
                  , owner       = 1
                  , description = Text.empty
                  , date        = pubDate
                  , categories  = []
                  , visibility  = Visible }

    put ps { nextPinId = succ nextPinId , pins = IxSet.insert pin pins }

updatePin pin = do
    ps@Pins{..} <- get
    put ps { pins = IxSet.updateIx (pinId pin) pin pins } -- Try saying that three times fast

pinById pid = do
    Pins{..} <- ask
    return . getOne $ pins @= pid
