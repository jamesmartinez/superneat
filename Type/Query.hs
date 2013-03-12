{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}

module Type.Query where

import Type.Common
import Type.User
import Type.Superneat
import Type.Pin


allPins :: Query Superneat [Pin]
allPins = do
    Pins{..} <- fmap statePins ask
    return $ toList pins

newPin :: UserId -> Text -> UTCTime -> [PinCategory] -> Visibility -> Update Superneat ()
newPin owner description date categories visibility = do
    state <- get

    let ps@Pins{..} = statePins state

    let pin = Pin { pinId = nextPinId
                  , owner = owner
                  , description = description
                  , date = date
                  , categories = categories
                  , visibility = visibility 
                  }

    put $ Superneat { statePins  = ps { nextPinId = succ nextPinId , pins = insert pin pins}
                    , stateUsers = stateUsers state}


-- These have not yet been fixed to dive into the Superneat state
-- updatePin :: Pin -> Update Pins ()
-- updatePin pin = do
--     ps@Pins{..} <- get
--     put ps { pins = updateIx (pinId pin) pin pins } -- Try saying that three times fast

-- pinById :: PinId -> Query Pins (Maybe Pin)
-- pinById pid = do
--     Pins{..} <- ask
--     return . getOne $ pins @= pid

-- $(makeAcidic ''Pins [ 'newPin
--                     , 'updatePin
--                     , 'pinById
--                     , 'allPins
--                     ])


$(makeAcidic ''Superneat [ 'allPins
                         , 'newPin ])
