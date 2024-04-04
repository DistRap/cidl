module Cidl.Utils
  ( dictModuleName
  , firstCap
  , firstLower
  , snakeToCamel
  ) where

import qualified Data.Char
import Control.Lens ((^.))

import Cidl.Dict
import Cidl.Lens

dictModuleName :: Dict -> String
dictModuleName d = firstCap $ snakeToCamel (d ^. name)

-- | Capitalize first char
firstCap
  :: String
  -> String
firstCap (s:ss) = (Data.Char.toUpper s) : ss
firstCap []     = []

-- | Lower first char
firstLower
  :: String
  -> String
firstLower (s:ss) = (Data.Char.toLower s) : ss
firstLower []     = []

-- | Snake case to camel case
snakeToCamel
  :: String
  -> String
snakeToCamel ('_':'i':[]) = []
snakeToCamel ('_':[]) = []
snakeToCamel ('_':a:as) =
  (Data.Char.toUpper a) : snakeToCamel as
snakeToCamel (a:as) =
  a : snakeToCamel as
snakeToCamel [] = []
