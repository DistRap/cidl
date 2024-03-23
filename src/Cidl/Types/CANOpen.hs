-- Mapping from our AST/Base types
-- to CANOpen ObjectType and DataType
module Cidl.Types.CANOpen
  ( ObjectType(..)
  , DataType(..)
  , toCANOpenType
  ) where

import Cidl.Types.AST

-- | CANOpen dictionary object types
data ObjectType
  = ObjectType_Null
  | ObjectType_Reserved1
  | ObjectType_Domain
  | ObjectType_Reserved3
  | ObjectType_Reserved4
  | ObjectType_DefType
  -- ^ Type definition
  | ObjectType_DefStruct
  -- ^ New record type definition
  | ObjectType_Var
  -- ^ Single value, most common object type
  | ObjectType_Array
  -- ^ Array of values, subindex 0 (uint8) is array length.
  -- Elements are of the same type
  | ObjectType_Record
  -- ^ Multi-data field, subindex 0 (uint8) is record length.
  -- Elements are of varying types
  deriving (Eq, Enum, Ord, Show)

-- | CANOpen dictionary static data types.
-- Only ObjectType_DefType are listed here
data DataType
  = DataType_Reserved0x0
  | DataType_Boolean
  | DataType_Int8
  | DataType_Int16
  | DataType_Int32
  | DataType_Word8
  | DataType_Word16
  | DataType_Word32
  | DataType_Float
  | DataType_VisibleString
  | DataType_OctetString
  | DataType_UnicodeString
  | DataType_TimeOfDay
  | DataType_TimeDifference
  | DataType_Reserved0xE
  | DataType_Domain
  | DataType_Int24
  | DataType_Double
  | DataType_Int40
  | DataType_Int48
  | DataType_Int56
  | DataType_Int64
  | DataType_Word24
  | DataType_Reserved0x17
  | DataType_Word40
  | DataType_Word48
  | DataType_Word56
  | DataType_Word64
  deriving (Eq, Enum, Ord, Show)

data CANOpenType = CANOpenType
  { canOpenTypeObjectType :: ObjectType
  , canOpenTypeDataType :: Maybe DataType
  }
  deriving (Eq, Ord, Show)

-- | Get EDS object type and data type IDs
toCANOpenType
  :: Type
  -> CANOpenType
toCANOpenType (RecordType _ _)
  = CANOpenType ObjectType_DefStruct Nothing
toCANOpenType (ArrayType _ _ _)
  = CANOpenType ObjectType_Array Nothing
toCANOpenType (VarArrayType _ )
  = CANOpenType ObjectType_Array Nothing
toCANOpenType (PrimType pt)
  = CANOpenType ObjectType_Var (Just $ primDataType pt)

primDataType
  :: PrimType
  -> DataType
primDataType (Newtype _ pt) = primDataType pt
primDataType (EnumType _ bits _) = primDataType (AtomType $ AtomWord bits)
primDataType (AtomType atom) = atomDataType atom

atomDataType
  :: Atom
  -> DataType
atomDataType (AtomInt Bits8) = DataType_Int8
atomDataType (AtomInt Bits16) = DataType_Int16
atomDataType (AtomInt Bits32) = DataType_Int32
atomDataType (AtomInt Bits64) = DataType_Int64
atomDataType (AtomWord Bits8) = DataType_Word8
atomDataType (AtomWord Bits16) = DataType_Word16
atomDataType (AtomWord Bits32) = DataType_Word32
atomDataType (AtomWord Bits64) = DataType_Word64
atomDataType AtomFloat = DataType_Float
atomDataType AtomDouble = DataType_Double
