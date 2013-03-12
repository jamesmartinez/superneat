{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, RecordWildCards #-}

module Type.Superneat where

import Type.Common
import Type.Pin
import Type.User


data Superneat = Superneat { stateUsers :: Users 
                           , statePins  :: Pins }
               deriving (Typeable)

$(deriveSafeCopy 0 'base ''Superneat)

emptySuperneat :: Superneat
emptySuperneat = Superneat noUsers noPins

