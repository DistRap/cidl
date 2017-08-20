{-# LANGUAGE TemplateHaskell #-}
module Cidl.Dict.AST where

import Cidl.Types.AST
import Lens.Family2.TH

data Dict
  = Dict
  { _dictName :: String
  , _dictParents :: [Dict]
  , _dictTypes :: [Type]
  , _dictEntries :: [Entry]
  }
  deriving (Eq, Show)

$(makeLenses ''Dict)
