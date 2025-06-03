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
import qualified Cidl.Types.Base

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
    , text "{-# LANGUAGE FlexibleContexts #-}"
    , text "{-# LANGUAGE RecordWildCards #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , empty
    , text "module"
      <+> im (dictModuleName dict)
      <+> text "where"
    , empty
    , stack
        typeImports
    , text "import Network.CANOpen.Class (MonadNode(..))"
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
    [     variables entry
      </> complexAliases entry
    | entry <- Cidl.Dict.allEntries dict ]

variable
  :: Entry
  -> Maybe Int -- ^ Sub-index
  -> Doc
variable e@Entry{..} mSub =
  let
    camelName = Cidl.Utils.snakeToCamel entryName
    capName = Cidl.Utils.firstCap camelName
  in
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
              <+> text (maybe "0" show mSub)
            ,     text "variablePerm"
              <+> equals
              <+> buildPerm entryAccess
            ]
    , alias e
    ]

-- | Calls variable multiple times for complex types
variables
  :: Entry
  -> Doc
variables e =
  case entryTyp e of
    PrimType _primType ->
      variable e Nothing
    RecordType _recordName subEntries ->
          variable
            (e
              { entryTyp = Cidl.Types.Base.uint8
              , entryName = entryName e <> "_count"
              }
            )
            Nothing
      </> stack
            [ variable
                (sub
                  { entryName = entryName e <> "_" <> entryName sub })
                  (Just subIdx)
            | (sub, subIdx) <- zip subEntries [(1 :: Int) .. ]
            ]
    ArrayType _arrName arrLen arrTyp ->
          variable
            (e
              { entryTyp = Cidl.Types.Base.uint8
              , entryName = entryName e <> "_count"
              }
            )
            Nothing
      </> stack
            [ variable
                (e
                  { entryTyp = arrTyp
                  , entryName = entryName e <> show subIdx
                  }
                )
                (Just subIdx)
            | subIdx <- [(1 :: Int) .. arrLen ]
            ]

    -- VarArrayType hodl
    _ -> empty

buildPerm :: Perm -> Doc
buildPerm p =
     text "Permission_"
  <> text (show p)

-- | sdoRead/write alias
alias
  :: Entry
  -> Doc
alias Entry{..} =
  let
    camelName = Cidl.Utils.snakeToCamel entryName
    capName = Cidl.Utils.firstCap camelName
  in
    stack
      [ if (entryAccess `elem` [Read, ReadWrite])
        then stack
          [     text "read"
            <>  text capName
            <+> text "::"
            <+> text "MonadNode m"
            <+> text "=>"
            <+> text "m"
            <+> text (typeHaskellType entryTyp)
          ,     text "read"
            <>  text capName
            <+> text "="
            <+> text "sdoRead"
            <+> text camelName
          ]
        else empty
      , if (entryAccess `elem` [Write, ReadWrite])
        then stack
          [     text "write"
            <>  text capName
            <+> text "::"
            <+> text "MonadNode m"
            <+> text "=>"
            <+> text (typeHaskellType entryTyp)
            <+> text "->"
            <+> text "m ()"
          ,     text "write"
            <>  text capName
            <+> text "="
            <+> text "sdoWrite"
            <+> text camelName
          ]

        else empty
      ]

-- | sdoRead/Write aliases for complex types
-- Generated iff all fields are readable/writeable
complexAliases
  :: Entry
  -> Doc
complexAliases e =
  let
    camelName = Cidl.Utils.snakeToCamel (entryName e)
    capName = Cidl.Utils.firstCap camelName
  in
  case entryTyp e of
    PrimType _primType -> empty
    RecordType _recordName subEntries ->
      stack
        [ if all (\s -> entryAccess s `elem` [Read, ReadWrite]) subEntries
          then stack
            [     text "read"
              <>  text capName
              <+> text "::"
              <+> text "MonadNode m"
              <+> text "=>"
              <+> text "m"
              <+> text (typeHaskellType (entryTyp e))
            ,     text "read"
              <>  text capName
              <+> text "= do"
            , indent 2
                $ stack
                  [     text (Cidl.Utils.snakeToCamel $ entryName s)
                    <+> text "<-"
                    <>  text
                          (Cidl.Utils.snakeToCamel
                            $  "read_"
                            <> entryName e
                            <> "_"
                            <> entryName s
                          )
                  | s <- subEntries
                  , entryAccess s `elem` [Read, ReadWrite]
                  ]
            , indent 2
                $   text "pure"
                <+> text (typeHaskellType (entryTyp e))
                <>  text "{..}"
            ]
          else empty
        , if all (\s -> entryAccess s `elem` [Write, ReadWrite]) subEntries
          then stack
            [     text "write"
              <>  text capName
              <+> text "::"
              <+> text "MonadNode m"
              <+> text "=>"
              <+> text (typeHaskellType (entryTyp e))
              <+> text "->"
              <+> text "m ()"
            ,     text "write"
              <>  text capName
              <+> text (typeHaskellType (entryTyp e))
              <>  text "{..}"
              <+> text "= do"
            , indent 2
                $ stack
                  [     text
                          (Cidl.Utils.snakeToCamel
                            $  "write_"
                            <> entryName e
                            <> "_"
                            <> entryName s
                          )
                    <+> text (Cidl.Utils.snakeToCamel $ entryName s)
                  | s <- subEntries
                  , entryAccess s `elem` [Read, ReadWrite]
                  ]
            ]
          else empty
        ]

    -- VarArrayType hodl
    _ -> empty
