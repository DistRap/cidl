{-# LANGUAGE OverloadedStrings #-}
module Cidl.Backend.Elm.Type (typeModule) where

import Data.List (intercalate,nub)

import Cidl.Backend.Elm.Common

import Cidl.Types --(Type(..),PrimType(..),typeLeaves)

import Ivory.Artifact
    (Artifact,artifactPath,artifactText)

import Text.PrettyPrint.Mainland
import Lens.Family2

-- | Produce an Elm module for a 'Type'
typeModule :: Namespace -> Type -> Artifact
typeModule ns t =
  artifactPath (intercalate "/" mp) $
  artifactText ((cappedName t) ++ ".elm") $
  prettyLazyText 1000 $
  stack $
    [ "module" <+> tm (cappedName t) <+> "exposing (..)"
    , empty
    , stack (imports ++ [
                 "import" <+> mkQName ns "Utils" <+> "as Utils"
               , "import Json.Decode"
               , "import Json.Decode.Pipeline exposing (required)"
               , "import Json.Encode"
               ])
    , empty
    , typeDecl mp t
    ]
  where
    imports = map (importDecl tm) $ nub $ map importType $ typeLeaves t
    tm = mkQName mp
    mp = ns ++ ["Types"]

-- | Produce the body of the module for a 'Type' differently depending
-- on whether it's a struct, enum, or newtype
typeDecl :: ModulePath -> Type -> Doc
typeDecl mp t@(RecordType _ es) = stack
  [ -- the type definition itself
    "type alias" <+> tname <+> equals
  , indent 2 $ encloseStack lbrace rbrace comma
      [ text (e ^. name) <+> colon <+> elmTypeQName mp (e ^. typ)
      | e <- es ]
  , empty
    -- JSON encoder
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> text "x" <+> equals
  , indent 2 $ stack
      [ "Json.Encode.object"
      , indent 2 $ encloseStack lbracket rbracket comma
        [ parens (dquotes (text (e ^. name))
            <> comma <+> typeEncoder mp (e ^. typ) <+> "x" <> dot <> (text (e ^. name)))
        | e <- es ]
      ]
  , empty
    -- JSON decoder
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
  , "decode" <+> equals <+> "Json.Decode.Pipeline.decode" <+> tname
  , indent 2 $ stack $
      [ "|>" <+> parens ("required" <+> dquotes (text (e ^. name)) <+> typeDecoder mp (e ^. typ))
      | e <- es ]
  , empty
    -- arbitrary "initialized" value
  , "{-|" <+> tname <+> "initialized with (arbitrary) default values -}"
  , "init" <+> colon <+> tname
  , "init" <+> equals
  , indent 2 $ encloseStack lbrace rbrace comma
      [ text (e ^. name) <+> equals <+> typeInit mp (e ^. typ)
      | e <- es ]
  ]
  where
  tname = text (cappedName t)

typeDecl mp t@(PrimType (Newtype _ n)) = stack
  [ -- the type definition itself
    "type alias" <+> tname <+> equals <+> elmTypeQName mp (PrimType n)
  , empty
    -- JSON encoder
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> equals <+> primTypeEncoder mp n
  , empty
    -- JSON decoder
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
  , "decode" <+> equals <+> primTypeDecoder mp n
  , empty
    -- arbitrary "initialized" value
  , "{-|" <+> tname <+> "initialized with (arbitrary) default values -}"
  , "init" <+> colon <+> tname
  , "init" <+> equals <+> primTypeInit mp n
  ]
  where
  tname = text (cappedName t)

typeDecl _  t@(PrimType (EnumType _ _ es)) = stack $
  [ -- the type definition itself
    text "type" <+> tname
  , indent 2 $ encloseStack equals empty "|"
      [ text (toCapped i)
      | (i, _) <- es ]
    -- JSON encoder
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  ] ++
  encodeBody ++
  [ empty
    -- JSON decoder
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
  , "decode" <+> equals
  , indent 2 $ stack [
        "Utils.customDecoder" <+> "Json.Decode.string"
        <+> lparen <> "\\tag ->"
      , indent 2 $ stack [
            "case tag of"
          , indent 2 $ stack $
              [ dquotes (text (toCapped i)) <+> "->"
                <+> "Ok" <+> text (toCapped i)
              | (i, _) <- es ] ++
              [ "str -> Err (\"unrecognized" <+> tname
                <+> "tag: \" ++ str)" <> rparen
              ]
          ]
      ]
  , empty
    -- arbitrary "initialized" value
  , "{-|" <+> tname <+> "initialized with (arbitrary) default values -}"
  , "init" <+> colon <+> tname
  , "init" <+> equals <+> text (toCapped firstConstr)
  ]
  where
  -- Aeson encodes sum types with one variant as `[]`
  encodeBody =
    case es of
      [_] -> [ "encode" <+> "x" <+> equals <+> "Json.Encode.list []" ]
      _ -> [ "encode" <+> "x" <+> equals
           , indent 2 $ stack [
               "case x of"
               , indent 2 $ stack
                 [ text (toCapped i) <+> "->"
                   <+> "Json.Encode.string" <+> dquotes (text (toCapped i))
                 | (i, _) <- es ]
               ]
           ]
  tname = text (cappedName t)
  firstConstr =
    case es of
      [] -> error ("empty enumeration " ++ cappedName t)
      (i, _) : _ -> i

typeDecl _ t = error ("typeDecl: cannot create Elm decl for type " ++ show t)
