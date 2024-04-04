
module Cidl.Backend.Ivory.Types where

import Data.List (intercalate, nub)
import Cidl.Types
import Cidl.Lens
import Ivory.Artifact
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class
import Control.Lens ((^.))

import qualified Cidl.Utils

typeUmbrella :: [String] -> [Type] -> Artifact
typeUmbrella modulepath ts =
  artifactPath (intercalate "/" modulepath) $
  artifactText ("Types.hs") $
  prettyLazyText 1000 $
  stack $
    [ text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , text "module" <+> typeModulePath modulepath "Types" <+> text "where"
    , empty
    , text "import Ivory.Language"
    , stack
        [ importDecl (typeModulePath (modulepath ++ ["Types"])) (importType t)
        | t <- ts ]
    , empty
    , text "typeModules :: [Module]"
    ]
    ++ maybeTypeModules
  where
    -- in case there's only an array defined, this can
    -- end up (m)empty, preserve for now but maybe skip
    -- generating the file altogether
    maybeTypeModules =
      if null mods
      then
        [ text "typeModules = mempty" ]
      else
        [ text "typeModules ="
        , indent 2
           $ encloseStack
               lbracket
               rbracket
               comma
               mods
        ]

    mods =
      [ text tname <> dot
        <> text (userEnumValueName tname) <> text "TypesModule"
      | t <- ts
      , let tname = typeModuleName t
      , notArray t
      ]

notArray :: Type -> Bool
notArray (ArrayType _ _ _) = False
notArray _ = True

-- invariant: only make a typeModule from a RecordType, ArrayType, Newtype, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: [String] -> Type -> Artifact
typeModule modulepath t =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName t) ++ ".hs") $
  prettyLazyText 1000 $
  stack
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE TypeOperators #-}"
    , text "{-# LANGUAGE QuasiQuotes #-}"
    , text "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , text "{-# LANGUAGE FlexibleInstances #-}"
    , text "{-# OPTIONS_GHC -fno-warn-orphans #-}"
    , empty
    , text "module"
      <+> typeModulePath modulepath (typeModuleName t)
      <+> text "where"
    , empty
    , stack (imports ++
              [ text "import Ivory.Language"
              , emptyWhen (isArray t) (text "import Ivory.Serialize")
              ])
    , empty
    , typeDecl t
    ]
  where
  imports = map (importDecl (typeModulePath modulepath))
          $ nub
          $ map importType
          $ typeLeaves t

typeModulePath :: [String] -> String -> Doc
typeModulePath modulepath mname = mconcat $ punctuate dot
                                          $ map text (modulepath ++ [mname])

typeImportedIvoryType :: Type -> String
typeImportedIvoryType t@(PrimType (Newtype tn _)) =
  userTypeModuleName tn ++ "." ++ typeIvoryType t
typeImportedIvoryType t@(PrimType (EnumType tn _ _)) =
  userTypeModuleName tn ++ "." ++ typeIvoryType t
typeImportedIvoryType t = typeIvoryType t

-- | The context determines whether we format promoted types with or
-- without tick marks
data SyntaxContext = Concrete | Embedded deriving Show

typeIvoryArea :: SyntaxContext -> Type -> Doc
typeIvoryArea sc t =
  case t of
    RecordType _ _ ->
      parens (text (typeIvoryType t))
    VarArrayType t' ->
      parens (text (typeIvoryType t'))
    ArrayType _ _ _ ->
      parens (text (typeIvoryType t) <> text "." <> text (typeIvoryType t))
    PrimType (AtomType _) ->
      parens (text stored <+> text (typeIvoryType t))
    PrimType _ ->
      parens (text stored <+> text (typeIvoryType t) <> dot <> text (typeIvoryType t))
  where
    stored = case sc of
      Concrete -> "Stored"
      Embedded -> "'Stored"

typeIvoryAreaStructQQ :: Type -> Doc
typeIvoryAreaStructQQ (RecordType n _) = text "Struct" <+> text (userTypeStructName n)
typeIvoryAreaStructQQ t = typeIvoryArea Concrete t

typeIvoryType :: Type -> String
typeIvoryType (RecordType tn _) = "'Struct \"" ++ userTypeStructName tn ++ "\""
typeIvoryType (ArrayType tn _ _) = userTypeModuleName tn
typeIvoryType (VarArrayType t) = typeIvoryType t
typeIvoryType (PrimType (Newtype tn _)) = userTypeModuleName tn
typeIvoryType (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeIvoryType (PrimType (AtomType a)) = case a of
  AtomBool -> "IBool"
  AtomInt Bits8  -> "Sint8"
  AtomInt Bits16 -> "Sint16"
  AtomInt Bits32 -> "Sint32"
  AtomInt Bits64 -> "Sint64"
  AtomWord Bits8  -> "Uint8"
  AtomWord Bits16 -> "Uint16"
  AtomWord Bits32 -> "Uint32"
  AtomWord Bits64 -> "Uint64"
  AtomFloat -> "IFloat"
  AtomDouble -> "IDouble"

typeModuleName :: Type -> String
typeModuleName (RecordType tn _) = userTypeModuleName tn
typeModuleName (ArrayType tn _ _ ) = userTypeModuleName tn
typeModuleName (VarArrayType t) = typeModuleName t
typeModuleName (PrimType (Newtype tn _)) = userTypeModuleName tn
typeModuleName (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeModuleName (PrimType (AtomType _)) = error "do not take typeModuleName of an AtomType"

userTypeModuleName :: String -> String
userTypeModuleName =
    Cidl.Utils.firstCap
  . userEnumValueName

userEnumValueName :: String -> String
userEnumValueName =
    Cidl.Utils.firstLower
  . Cidl.Utils.snakeToCamel

userTypeStructName :: String -> String
userTypeStructName =
    Cidl.Utils.firstLower
  . drop_t_suffix
  where
  drop_t_suffix []     = []
  drop_t_suffix ('_':'t':[]) = []
  drop_t_suffix (a:as) = a : drop_t_suffix as

ivoryPackageName :: Type -> String
ivoryPackageName (RecordType tname _) = userEnumValueName tname ++ "TypesModule"
ivoryPackageName (PrimType (Newtype tname _)) = userEnumValueName tname ++ "TypesModule"
ivoryPackageName (PrimType (EnumType tname _ _)) = userEnumValueName tname ++ "TypesModule"
ivoryPackageName _ = error "can't take ivoryPackageName of builtin type"
qualifiedIvoryPackageName :: Type -> String
qualifiedIvoryPackageName t = typeModuleName t ++ "." ++ ivoryPackageName t


typeDecl :: Type -> Doc
typeDecl t@(RecordType tname es) = stack
  [ text "[ivory|"
  , text "struct" <+> structname
  , indent 2 $ encloseStack lbrace rbrace semi
      [ text (e ^. name) <+> colon <> colon
       <+> typeIvoryAreaStructQQ (e ^. typ)
      | e <- es ]
  , text "|]"
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes structname <+> text "$"
  , indent 2 $ text "packStruct" <+> encloseStack lbracket rbracket comma
                [ text "packLabel" <+> text (e ^. name)
                | e <- es]
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (ivoryPackageName t) <+> text ":: Module"
  , text (ivoryPackageName t) <+> equals
    <+> text "package" <+> dquotes (structname <> text "_types") <+> text "$ do"
  , indent 2 $ stack $
      [ text "defStruct"
        <+> parens (text "Proxy :: Proxy" <+> dquotes structname)
      , text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ] ++
      [ text "depend" <+> text (qualifiedIvoryPackageName dt)
      | dt <- typeLeaves t
      , isUserDefined dt
      , notArray dt
      ]

  ]
  where
  storedType = parens (text "'Struct" <+> dquotes structname)
  structname = text (userTypeStructName tname)
  packRep = text "pack" <> text (userTypeModuleName tname)

typeDecl (VarArrayType t) = typeDecl t
typeDecl (ArrayType tname len t) =
      text "type"
  <+> text typename
  <+> equals
  <+> text "'Array"
  <+> integer len
  <+> storedType (typeImportedIvoryType t)
  where
  typename = userTypeModuleName tname
  storedType stname = parens (text "'Stored" <+> text stname)

typeDecl t@(PrimType (Newtype tname n)) = stack
  [ text "newtype" <+> text typename <+> equals
  , indent 2 $ text typename <+> align
      (lbrace <+> text ("un" ++ typename ) <+> text "::"
       <+> text (typeImportedIvoryType (PrimType n))
       </> rbrace <+> typeDeriving (words ("IvoryType IvoryVar IvoryExpr " ++
                       "IvoryEq IvoryStore IvoryInit IvoryZeroVal Num")))
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes (text tname) <+> text "$"
  , indent 2 $ text "repackV" <+> text typename <+> text ("un" ++ typename) <+> text "packRep"
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (ivoryPackageName t) <+> text ":: Module"
  , text (ivoryPackageName t) <+> equals
    <+> text "package"
    <+> dquotes (text (userTypeStructName tname) <> text "_types")
    <+> text "$ do"
  , indent 2 $ stack $
      [ text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ] ++
      [ text "depend" <+> text (qualifiedIvoryPackageName dt)
      | dt <- typeLeaves t
      , isUserDefined dt
      ]
  ]
  where
  typename = userTypeModuleName tname
  storedType = parens (text "'Stored" <+> text typename)
  packRep = text "pack" <> text typename

typeDecl t@(PrimType (EnumType tname s es)) = stack
  [ text "newtype" <+> text typename <+> equals
  , indent 2 $ text typename <+> align
        (lbrace <+> text ("un" ++ typename) <+> text "::" <+>
         text bt </>
         rbrace <+> typeDeriving (words ("IvoryType IvoryVar IvoryExpr IvoryEq "
                                   ++ "IvoryStore IvoryInit IvoryZeroVal")))
  , empty
  , stack
      [ stack
        [ empty
        , text (userEnumValueName i) <+> colon <> colon <+> text typename
        , text (userEnumValueName i) <+> equals <+> text typename <+> ppr e
        ]
      | (i,e) <- es ]
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes (text tname) <+> text "$"
  , indent 2 $ text "repackV" <+> text typename <+> text ("un" ++ typename) <+> text "packRep"
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (ivoryPackageName t) <+> text ":: Module"
  , text (ivoryPackageName t) <+> equals
    <+> text "package"
    <+> dquotes (text (userTypeStructName tname) <> text "_types")
    <+> text "$ do"
  , indent 2 $ stack
      [ text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ]
  ]
  where
  typename = userTypeModuleName tname
  packRep = text "pack" <> text typename
  storedType = parens (text "'Stored" <+> text typename)
  bt = case s of
    Bits8 -> "Uint8"
    Bits16 -> "Uint16"
    Bits32 -> "Uint32"
    Bits64 -> "Uint64"

typeDecl a = error ("typeDecl: broken invariant, cannot create type for " ++ show a)

typeDeriving :: [String] -> Doc
typeDeriving cs = text "deriving" <+> parens (commasep (map text cs))

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: Type -> ImportType
importType (RecordType n _) = UserType n
importType (ArrayType n _ _) = UserType n
importType (VarArrayType t) = importType t
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType _)) = NoImport

isUserDefined :: Type -> Bool
isUserDefined t = case importType t of
  UserType _ -> True
  _ -> False

importPrefix :: ImportType -> Doc
importPrefix (UserType t) = text (userTypeModuleName t)
importPrefix _ = empty

importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import qualified" <+> mkpath (userTypeModuleName t)
      <+> text "as" <+> text (userTypeModuleName t)
importDecl _ NoImport = empty


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> align (l <+> d </> r)
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

onlyWhen :: Bool -> Doc -> Doc
onlyWhen True d = d
onlyWhen _ _ = empty

emptyWhen :: Bool -> Doc -> Doc
emptyWhen True _ = empty
emptyWhen _ d = d
