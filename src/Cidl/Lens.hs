{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Cidl.Lens where

import Control.Lens

import Cidl.Dict.AST
import Cidl.Types.AST

makeFields ''Dict
makeFields ''Entry
