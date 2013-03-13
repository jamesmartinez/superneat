{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

module Type.Superneat where

import Type.Common
import Type.Pin
import Type.User

data Superneat = Superneat { _users :: Table User 
                           , _pins  :: Table Pin }
                 deriving (Typeable)

emptySuperneat :: Superneat
emptySuperneat = Superneat empty empty

makeLenses ''Superneat
$(deriveSafeCopy 0 'base ''Superneat)


allPins :: Query Superneat (Table Pin)
allPins = view pins

newPin :: UserId -> Description -> UTCTime -> [PinCategory] -> Visibility -> Update Superneat ()
newPin o d t cs v = pins %= insert (Pin 0 o d t cs v)

$(makeAcidic ''Superneat [ 'allPins , 'newPin ])
