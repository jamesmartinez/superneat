module Type.Common
       ( Text
       , UTCTime
       , SafeCopy
       , Typeable
       , Data
       , Indexable
       , base
       , deriveSafeCopy
       , Acid.Update
       , Acid.Query
       , Acid.makeAcidic
       , get
       , put
       , modify
       , ask
       , asks
       , module Data.Table
       , module Control.Lens
       , (<$>)
       ) where

import Control.Applicative
import Data.Time.Clock (UTCTime)
import Data.Text as Text (Text)
import Data.Data (Typeable, Data)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Data.Acid as Acid (Update, Query, makeAcidic)
import Control.Monad.State (get, put, modify)
import Control.Monad.Reader (ask, asks)
import Control.Lens
import Data.Table
