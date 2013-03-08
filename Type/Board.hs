{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}

module Type.Board where

import Type.Common

data Board = Board { boardPins :: [PinId], boardOwner :: UserId, followers :: [UserId] }
