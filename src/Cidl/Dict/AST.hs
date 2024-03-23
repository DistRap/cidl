module Cidl.Dict.AST where

import Data.Default.Class (Default(def))
import Cidl.Types.AST

data Dict
  = Dict
  { dictName :: String
  , dictParents :: [Dict]
  , dictTypes :: [Type]
  , dictEntries :: [Entry]
  }
  deriving (Eq, Show)

instance Default Dict where
  def =
    Dict
    { dictName = "default"
    , dictParents = mempty
    , dictTypes = mempty
    , dictEntries = mempty
    }

