{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}
module Pin where

import Data.Text as Text (Text, empty)
import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Data.Acid (Update, Query, makeAcidic)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.IxSet as IxSet (Indexable, IxSet, empty, ixSet, ixFun, insert, (@=), getOne, updateIx, toList)

-- All the crunchy stuff
newtype UserId = UserId { unUserId :: Integer }
               deriving (Eq, Ord, Num, Typeable, SafeCopy)

newtype UserToken = UserToken Text
                  deriving (Eq, Ord, SafeCopy)

newtype PinId = PinId { unPinId :: Integer }
              deriving (Eq, Ord, Num, Enum, Typeable, SafeCopy)

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
            deriving (Typeable)

$(deriveSafeCopy 0 'base ''Pins)


-- Initial state
noPins = Pins { nextPinId = 0, pins = ixSet [] }
  
--- Getters and setters

-- Overwrites pinId, is this bad? 
newPin :: Pin -> Update Pins ()  
newPin pin = do
    ps@Pins{..} <- get
    put ps { nextPinId = succ nextPinId
           , pins      = IxSet.insert (pin {pinId = nextPinId}) pins
           }

updatePin :: Pin -> Update Pins ()
updatePin pin = do
    ps@Pins{..} <- get
    put ps { pins = IxSet.updateIx (pinId pin) pin pins } -- Try saying that three times fast

pinById :: PinId -> Query Pins (Maybe Pin)
pinById pid = do
    Pins{..} <- ask
    return . getOne $ pins @= pid

allPins :: Query Pins ([Pin])
allPins = do
    Pins{..} <- ask
    return $ toList pins
$(makeAcidic ''Pins [ 'newPin
                    , 'updatePin
                    , 'pinById
                    , 'allPins
                    ])
