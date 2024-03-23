
module Cidl.Types.Base
  ( uint8
  , uint16
  , uint32
  , uint64
  , sint8
  , sint16
  , sint32
  , sint64
  , bool
  , float
  , double
  , baseTypeEnv
  ) where

import Cidl.Types.AST

uint8  :: Type
uint8  = PrimType (AtomType (AtomWord Bits8))
uint16 :: Type
uint16 = PrimType (AtomType (AtomWord Bits16))
uint32 :: Type
uint32 = PrimType (AtomType (AtomWord Bits32))
uint64 :: Type
uint64 = PrimType (AtomType (AtomWord Bits64))

sint8  :: Type
sint8  = PrimType (AtomType (AtomInt  Bits8))
sint16 :: Type
sint16 = PrimType (AtomType (AtomInt  Bits16))
sint32 :: Type
sint32 = PrimType (AtomType (AtomInt  Bits32))
sint64 :: Type
sint64 = PrimType (AtomType (AtomInt  Bits64))

bool :: Type
bool = PrimType (EnumType "bool" Bits8 [("false", 0), ("true", 1)])

float :: Type
float = PrimType (AtomType AtomFloat)

double :: Type
double = PrimType (AtomType AtomDouble)

baseTypeEnv :: TypeEnv
baseTypeEnv = TypeEnv
  [ ( "uint8" , uint8)
  , ( "uint16", uint16)
  , ( "uint32", uint32)
  , ( "uint64", uint64)
  , ( "sint8" , sint8)
  , ( "sint16", sint16)
  , ( "sint32", sint32)
  , ( "sint64", sint64)
  , ( "bool"  , bool)
  , ( "float" , float)
  , ( "double", double)
  ]
