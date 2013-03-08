{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, EmptyDataDecls #-}
module Type.Common where

import Data.Text as Text (Text, empty)
import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Data.Acid (Update, Query, makeAcidic)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.IxSet as IxSet (Indexable, IxSet, empty, ixSet, ixFun, insert, (@=), getOne, updateIx, toList)

