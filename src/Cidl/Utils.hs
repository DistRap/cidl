module Cidl.Utils where

import Data.Char
import Cidl.Dict
import Lens.Family2

dictModuleName :: Dict -> String
dictModuleName d = aux (d ^. dictName)
  where
  aux :: String -> String
  aux = first_cap . u_to_camel
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'i':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []
