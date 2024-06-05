{-# LANGUAGE RecordWildCards #-}
module Cidl.Backend.Haskell.Dict where

import Data.List (intercalate, nub)

import Cidl.Dict (Dict)
import Cidl.Backend.Haskell.Types
import Cidl.Types.AST (Entry(..), Perm(..), Type(..))
import Cidl.Utils
import Ivory.Artifact
import Text.PrettyPrint.Mainland

import qualified Cidl.Dict

interfaceModule
  :: [String]
  -> Dict
  -> Artifact
interfaceModule modulepath dict =
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
        typeImports
    , text "import Network.CANOpen.Types"
    , empty
    , dictVarsAndFunctions dict
    ]
  where
  im mname =
    mconcat
    $ punctuate dot
    $ map
        text
        (modulepath ++ [mname])

  tm mname =
    mconcat
    $ punctuate dot
    $ map
        text
        (typepath modulepath ++ ["Types", mname])
    where
    typepath = reverse . drop 1 . reverse

  typeImports =
    map
      (\a -> importDecl tm a </> qualifiedImportDecl tm a)
    $ nub
    $ map importType
    $ Cidl.Dict.allTypes dict

dictVarsAndFunctions
  :: Dict
  -> Doc
dictVarsAndFunctions dict =
  stack
    [ variable entry
    | entry <- Cidl.Dict.allEntries dict ]

variable
  :: Entry
  -> Doc
variable Entry{..} =
  let
    camelName = Cidl.Utils.snakeToCamel entryName
    capName = Cidl.Utils.firstCap camelName
  in
  case entryTyp of
    PrimType _primType ->
      stack
      [     text camelName
        <+> text ":: Variable"
        <+> text (typeHaskellType entryTyp)
      ,     text camelName
        <+> equals
        <+> text "Variable"
      , indent 2
          $ encloseStack
              lbrace
              rbrace
              comma
              [     text "variableName"
                <+> equals
                <+> dquotes (text capName)
              ,     text "variableMux"
                <+> equals
                <+> text "Mux"
                <+> text (Cidl.Utils.fmtHex entryIndex)
                <+> text "0"
              ,     text "variablePerm"
                <+> equals
                <+> buildPerm entryAccess
              ]
      ]

    _ -> empty

buildPerm :: Perm -> Doc
buildPerm p =
     text "Permission_"
  <> text (show p)
