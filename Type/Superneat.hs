{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

module Type.Superneat where

import Type.Common
import Type.Pin
import Type.User


data Superneat = Superneat { userState :: Users 
                           , pinState  :: Pins }
                 deriving (Typeable)

$(deriveSafeCopy 0 'base ''Superneat)

emptySuperneat :: Superneat
emptySuperneat = Superneat noUsers noPins

allPins :: Query Superneat [Pin]
allPins = do
    Pins{..} <- fmap pinState ask
    return $ toList pins

newPin :: UserId -> Text -> UTCTime -> [PinCategory] -> Visibility -> Update Superneat ()
newPin owner description date categories visibility = do
    state <- get

    let ps@Pins{..} = pinState state

    let pin = Pin { pinId = nextPinId
                  , owner = owner
                  , description = description
                  , date = date
                  , categories = categories
                  , visibility = visibility 
                  }

    put $ Superneat { pinState  = ps { nextPinId = succ nextPinId , pins = insert pin pins}
                    , userState = userState state}

$(makeAcidic ''Superneat [ 'allPins , 'newPin ])
