{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}
module Type.Pin where

import Type.Common
import Type.User (UserId)


newtype PinId = PinId { unPinId :: Integer }
              deriving (Eq, Ord, Num, Enum, Typeable, SafeCopy)

newtype PinCategory = PinCategory Text
                    deriving (Eq, Ord, Typeable, SafeCopy)

newtype Description = Description Text
                    deriving (Eq, Ord, Typeable, SafeCopy)

data Visibility = Visible | Hidden
                deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''Visibility)
  
data Pin = Pin { _pinId       :: PinId
               , _owner       :: UserId
               , _description :: Description
               , _time        :: UTCTime 
               , _categories  :: [PinCategory]
               , _visibility  :: Visibility
               }
         deriving (Eq, Ord, Typeable)

makeLenses ''Pin
$(deriveSafeCopy 0 'base ''Pin)


instance Tabular Pin where
    type PKT Pin = PinId
    data Key k Pin b where
        PinIdK :: Key Primary Pin PinId
    data Tab Pin i = PinTab (i Primary PinId)

    fetch PinIdK = view pinId

    primary = PinIdK
    primarily PinIdK r = r

    mkTab f             = PinTab <$> f PinIdK
    forTab (PinTab x) f = PinTab <$> f PinIdK x
    ixTab (PinTab x) PinIdK = x

    autoTab = autoIncrement pinId
