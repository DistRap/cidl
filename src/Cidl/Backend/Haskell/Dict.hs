
module Cidl.Backend.Haskell.Dict where

import Data.List (intercalate, nub)

import Cidl.Dict
import Cidl.Backend.Haskell.Types
import Cidl.Utils
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: Bool -> [String] -> Dict -> Artifact
interfaceModule useAeson modulepath dict =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((dictModuleName dict) ++ ".hs") $
  prettyLazyText 1000 $
  stack $
    [ text "{-# LANGUAGE DeriveDataTypeable #-}"
    , text "{-# LANGUAGE DeriveGeneric #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , empty
    , text "module"
      <+> im (dictModuleName dict)
      <+> text "where"
    , empty
    , stack
        $  extraImports
        ++ typeImports
    , empty
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (typepath modulepath ++ ["Types", mname])
    where typepath = reverse . drop 1 . reverse

  typeImports = map (\a -> importDecl tm a </> qualifiedImportDecl tm a)
              $ nub
              $ map importType
              $ allTypes dict

  extraImports = [ text "import Data.Serialize"
                 , text "import Data.Typeable"
                 , text "import Data.Data"
                 , text "import GHC.Generics (Generic)"
                 , text "import qualified Test.QuickCheck as Q"
                 ] ++
                 [ text "import Data.Aeson (ToJSON,FromJSON)" | useAeson ]
