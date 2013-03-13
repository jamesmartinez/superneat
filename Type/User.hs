{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}

module Type.User where 

import Type.Common

newtype UserId = UserId { unUserId :: Integer }
               deriving (Num, Ord, Eq, Typeable, Data)

$(deriveSafeCopy 0 'base ''UserId)

data User = User { _userId   :: UserId
                 , _token    :: Text
                 , _nickname :: Text
                 , _joined   :: UTCTime }
            deriving (Eq, Ord, Typeable, Data)

makeLenses ''User

instance Tabular User where
    type PKT User = UserId
    data Key k User b where
        UserIdK :: Key Primary User UserId
    data Tab User i = UserTab (i Primary UserId)

    fetch UserIdK = view userId

    primary = UserIdK
    primarily UserIdK r = r

    mkTab f              = UserTab <$> f UserIdK
    forTab (UserTab x) f = UserTab <$> f UserIdK x
    ixTab (UserTab x) UserIdK = x

    autoTab = autoIncrement userId

$(deriveSafeCopy 0 'base ''User)
