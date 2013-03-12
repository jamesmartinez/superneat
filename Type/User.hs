{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Type.User where

import Type.Common
import Data.Time.Clock (UTCTime)


newtype UserId = UserId { unUserId :: Integer }
               deriving (Eq, Ord, Num, Typeable, SafeCopy)

newtype UserToken = UserToken Text
                  deriving (Eq, Ord, SafeCopy)

data User = User { userId   :: UserId
                 , token    :: UserToken
                 , nickname :: Text
                 , joined   :: UTCTime }
            deriving (Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''User)

data Users = Users { nextUserId :: UserId
                   , users      :: IxSet User }

instance Indexable User where
  empty = ixSet $ map ixFun $ [ \u -> [ userId u ] ]

$(deriveSafeCopy 0 'base ''Users)

noUsers :: Users
noUsers = Users { nextUserId = 0, users = ixSet [] }

$(makeAcidic ''Users [])
