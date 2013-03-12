{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}
module Type.Pin where

import Type.Common
import Type.User (UserId)
import Data.Time.Clock (UTCTime)


newtype PinId = PinId { unPinId :: Integer }
              deriving (Eq, Ord, Num, Enum, Typeable, SafeCopy)

newtype PinCategory = PinCategory Text
                      deriving (Eq, Ord, Typeable, SafeCopy)

data Visibility = Visible | Hidden
                deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''Visibility)
  
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
            deriving (Typeable)

$(deriveSafeCopy 0 'base ''Pins)


-- Initial state
noPins :: Pins
noPins = Pins { nextPinId = 0, pins = ixSet [] }

$(makeAcidic ''Pins [])
