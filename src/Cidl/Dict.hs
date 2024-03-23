
module Cidl.Dict
  ( module Cidl.Dict.AST
  , allEntries
  , allTypes
  , pdos
  , rpdos
  , tpdos
  ) where

import Data.Ord
import Data.List
import Cidl.Dict.AST
import Cidl.Types
import Cidl.Lens
import Control.Lens ((^.))

allEntries :: Dict -> [Entry]
allEntries d =
  concatMap allEntries (d ^. parents)
  ++ (d ^. entries)

allTypes :: Dict -> [Type]
allTypes d =
  nubBy (ignoreRecordInits) 
  $ concatMap allTypes (d ^. parents)
    ++ (d ^. types)
    ++ entryTypes
    ++ concatMap typeLeaves entryTypes
  where
  entryTypes = map (\e -> e ^. typ) (allEntries d)
  ignoreRecordInits (RecordType a _) (RecordType b _) = a == b
  ignoreRecordInits a b = a == b

pdos :: Dict -> [(Entry, Entry)]
pdos d = zip comms maps
  where
  comms = indexSort $ filter
    (\e -> (e ^. isRPDO || e ^. isTPDO) && (not $ e ^. isPDOMap))
    (allEntries d)

  maps = indexSort $ filter
    (\e -> e ^. isPDOMap) (allEntries d)

  indexSort = sortBy (comparing entryIndex)

rpdos :: Dict -> [(Entry, Entry)]
rpdos d = filter (\(p, _) -> p ^. isRPDO) (pdos d)

tpdos :: Dict -> [(Entry, Entry)]
tpdos d = filter (\(p, _) -> p ^. isTPDO) (pdos d)
