module Type.Common
       ( Text
       , UTCTime
       , SafeCopy
       , Typeable
       , Indexable
       , base
       , deriveSafeCopy
       , IxSet
       , IxSet.ixSet
       , IxSet.ixFun
       , IxSet.empty
       , IxSet.insert
       , IxSet.updateIx
       , IxSet.getOne
       , IxSet.toList
       , (@=)
       , Acid.Update
       , Acid.Query
       , Acid.makeAcidic
       , get
       , put
       , ask
       ) where

import Data.Time.Clock (UTCTime)
import Data.Text as Text (Text)
import Data.Data (Typeable)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)
import Data.Acid as Acid (Update, Query, makeAcidic)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.IxSet as IxSet (Indexable, IxSet, empty, ixSet, ixFun, insert, (@=), getOne, updateIx, toList)

