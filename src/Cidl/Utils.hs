module Cidl.Utils where

import qualified Data.Char
import Control.Lens ((^.))

import Cidl.Dict
import Cidl.Lens

dictModuleName :: Dict -> String
dictModuleName d = aux (d ^. name)
  where
  aux :: String -> String
  aux = first_cap . u_to_camel
  first_cap (s:ss) = (Data.Char.toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'i':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (Data.Char.toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []
