
module Cidl.Backend.Haskell.Dict where

import Control.Lens ((^.))
import Data.List (intercalate, nub)

import Cidl.Dict
import Cidl.Backend.Haskell.Types
import Cidl.Lens
import Cidl.Utils
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: Bool -> [String] -> Dict -> Artifact
interfaceModule useAeson modulepath d =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((dictModuleName d) ++ ".hs") $
  prettyLazyText 1000 $
  stack $
    [ text "{-# LANGUAGE DeriveDataTypeable #-}"
    , text "{-# LANGUAGE DeriveGeneric #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , empty
    , text "module"
      <+> im (dictModuleName d)
      <+> text "where"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (typepath modulepath ++ ["Types", mname])
    where typepath = reverse . drop 1 . reverse

  typeimports = map (\a -> importDecl tm a </> qualifiedImportDecl tm a)
              $ nub
              $ map importType
              $ (d ^. types)

  extraimports = [ text "import Data.Serialize"
                 , text "import Data.Typeable"
                 , text "import Data.Data"
                 , text "import GHC.Generics (Generic)"
                 , text "import qualified Test.QuickCheck as Q"
                 ] ++
                 [ text "import Data.Aeson (ToJSON,FromJSON)" | useAeson ]


--ifModuleName :: Interface -> String
--ifModuleName (Interface iname _ _) = aux iname
--  where
--  aux :: String -> String
--  aux = first_cap . u_to_camel
--  first_cap (s:ss) = (toUpper s) : ss
--  first_cap []     = []
--  u_to_camel ('_':'i':[]) = []
--  u_to_camel ('_':[]) = []
--  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
--  u_to_camel (a:as) = a : u_to_camel as
--  u_to_camel [] = []
