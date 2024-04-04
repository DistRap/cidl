module Cidl.Types
  ( module Cidl.Types.AST
  , module Cidl.Types.Base
  , lookupTypeName
  , insertType
  , typeLeaves
  , childTypes
  , sizeOf
  , basePrimType
  , typeName
  ) where

import Data.Tuple (swap)
import Data.List (nub)
import Cidl.Types.AST
import Cidl.Types.Base
import Cidl.Lens
import Control.Lens ((^.))

lookupTypeName :: TypeName -> TypeEnv -> Maybe Type
lookupTypeName tn te =
  case aux te of
    Just a -> Just a
    Nothing -> case aux baseTypeEnv of
      Just a -> Just a
      Nothing -> Nothing
  where
  aux (TypeEnv e) = lookup tn e

typeName :: Type -> TypeName
typeName (RecordType n _) = n
typeName (ArrayType n _ _) = n
typeName (VarArrayType t) = typeName t
typeName (PrimType (EnumType n _ _)) = n
typeName (PrimType (Newtype n _)) = n
typeName t@(PrimType (AtomType _)) =
  let TypeEnv bte = baseTypeEnv in
  case lookup t (map swap bte) of
    Just n -> n
    Nothing -> error "impossible: cannot find name for AtomType in baseTypeEnv"

insertType :: TypeName -> Type -> TypeEnv -> TypeEnv
insertType tn t e@(TypeEnv te) = case lookupTypeName tn e of
  Nothing -> TypeEnv ((tn,t):te)
  Just _ -> error ("insertType invariant broken: type " ++ tn ++ " already exists")

typeLeaves :: Type -> [Type]
typeLeaves (RecordType _ es) = nub [ e ^. typ | e <- es ]
typeLeaves (ArrayType _ _ t) = [t]
typeLeaves (VarArrayType t) = typeLeaves t
typeLeaves (PrimType (Newtype _ tn)) = [PrimType tn]
typeLeaves _ = []

childTypes :: Type -> [Type]
childTypes t = [t] ++ concat (map childTypes (typeLeaves t))

sizeOf :: Type -> Integer
sizeOf (RecordType _ es) = sum [ sizeOf (e ^. typ) | e <- es ]
sizeOf (ArrayType _ len t) = fromIntegral len * sizeOf t
-- sus, but sizeOf is not used anywhere yet for now
sizeOf (VarArrayType t) = sizeOf t
sizeOf (PrimType (Newtype _ tr)) = sizeOf (PrimType tr)
sizeOf (PrimType (EnumType _ bs _)) = bitsSize bs
sizeOf (PrimType (AtomType AtomBool)) = bitsSize Bits8
sizeOf (PrimType (AtomType (AtomInt bs))) = bitsSize bs
sizeOf (PrimType (AtomType (AtomWord bs))) = bitsSize bs
sizeOf (PrimType (AtomType AtomFloat)) = 4
sizeOf (PrimType (AtomType AtomDouble)) = 8

bitsSize :: Bits -> Integer
bitsSize Bits8  = 1
bitsSize Bits16 = 2
bitsSize Bits32 = 4
bitsSize Bits64 = 8

-- Reduce a newtype to the innermost concrete type
basePrimType :: PrimType -> PrimType
basePrimType (Newtype _ t) = basePrimType t
basePrimType a = a
