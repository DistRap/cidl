{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Cidl.Monad where

import MonadLib
import Data.Default.Class (Default(def))
import Control.Lens ((.~), (%~), (&))

import Cidl.Types.AST
import Cidl.Types.Base
import Cidl.Dict
import Cidl.Lens

newtype Entries a = Entries
  { unEntries :: WriterT [Entry] Id a }
  deriving (Functor, Monad, Applicative)

runEntries :: Entries () -> [Entry]
runEntries c = snd . runId $ runWriterT $ unEntries c

field
  :: String
  -> Type
  -> Entry
field n t =
  def
    & name .~ n
    & typ .~ t

ro, wo, rw, constant, reserved :: Entry -> Entry
ro = access .~ Read
wo = access .~ Write
rw = access .~ ReadWrite
constant = access .~ Const
reserved = access .~ Reserved

defaultNum :: Integer -> Entry -> Entry
defaultNum x = initial .~ (NumInit x)

-- | Fixed size array
array :: String -> Length -> Type -> Type
array n len t = ArrayType n len t

-- | Variable size array
varArray :: [Char] -> Length -> Type -> Type
varArray n len t = VarArrayType $ record ("var_array_" ++ n)
    [ field "arrayLengthL" uint8
    , field "arrayDataL" (array n len t)
    ]

-- | CANOpen Record type
record :: String -> [Entry] -> Type
record n fields = RecordType n fields

-- | @record@ alias
struct :: String -> [Entry] -> Type
struct = record

-- | Create an enum type
-- using smallest possible unsigned integer
-- type fitting all elements
enum
  :: String -- ^ Enum name in snake case
  -> [(Identifier, Integer)] -- ^ Enum values
  -> Type
enum enumName elems =
    PrimType
  $ EnumType
      enumName
      (smallestUint (length elems))
      elems
  where
    smallestUint :: Int -> Bits
    smallestUint numEls | numEls < 2^(8 :: Int) = Bits8
    smallestUint numEls | numEls < 2^(16 :: Int) = Bits16
    smallestUint numEls | numEls < 2^(32 :: Int) = Bits32
    smallestUint _      | otherwise = Bits64

-- | Enum element
enumVal
  :: Integer
  -> Identifier
  -> (Identifier, Integer)
enumVal n idx = (idx, n)

-- | Create a CANOpen dictionary
dict :: String -> WriterT [Entry] Id () -> Dict
dict n x =
  def
    & name .~ n
    & entries .~ genEntries
  where genEntries = runEntries $ Entries x

-- | Make this dictionary depend on another
-- dictionary
depend :: Dict -> Dict -> Dict
depend x = parents %~ (x:)

-- | Generate additional types
withTypes :: [Type] -> Dict -> Dict
withTypes ts = types %~ (++ts)

-- | Create a dictionary entry at some address
at :: WriterM m [Entry] => Integer -> Entry -> m ()
at addr e = put [ (index .~ addr) e ]

data NodeSpec = NodeSpec
  { vendorId :: Integer
  , productCode :: Integer
  , revisionNumber :: Integer
  , serialNumber :: Integer
  }

identity :: NodeSpec -> Type
identity NodeSpec{..} =
  record "identity"
    $ map mkfield
    $ zip
        ["vendor_id", "product_code", "revision_number", "serial_number"]
        [vendorId, productCode, revisionNumber, serialNumber]
  where
    mkfield (x, defNum) =
      field x uint32
        & defaultNum defNum

-- PDO communication parameters
-- ! PDOs are indexed from 1
pdoComm :: Integer -> Integer -> Type
pdoComm pdoOff pdoNum = record "pdo_comm_param"
  [ field "cob_id" uint32
      & defaultNum (pdoOff + 0x100 * pdoNum) -- + nodeId
  , field "transmission_type" uint8
  , field "inhibit_time" uint16
  , field "reserved" uint8 & reserved
  , field "event_timer" uint16
  ]

tpdo :: Integer -> Type
tpdo = pdoComm 0x80

rpdo :: Integer -> Type
rpdo = pdoComm 0x100

pdoMap :: Type
pdoMap = varArray "pdo_comm_map" 8 uint32

rpdoCommField :: Integer -> Entry
rpdoCommField n = field ("rpdo" ++ show n ++ "_params") (rpdo n) & (isRPDO .~ True)

tpdoCommField :: Integer -> Entry
tpdoCommField n = field ("tpdo" ++ show n ++ "_params") (tpdo n) & (isTPDO .~ True)

rpdoMapField :: Show a => a -> Entry
rpdoMapField n = field ("rpdo" ++ show n ++ "_map") pdoMap & (isRPDO .~ True) & (isPDOMap .~ True)

tpdoMapField :: Show a => a -> Entry
tpdoMapField n = field ("tpdo" ++ show n ++ "_map") pdoMap & (isTPDO .~ True) & (isPDOMap .~ True)

spec :: NodeSpec
spec = NodeSpec
  { vendorId = 0x48
  , productCode = 0x1
  , revisionNumber = 0x0
  , serialNumber = 0x1337
  }

interfaces :: [Dict]
interfaces =
  [ baseDict spec
  , cia401Dict
  , cia402Dict
  , reflowDict
  , testDict
  ]

baseDict :: NodeSpec -> Dict
baseDict nodeSpec = dict "base" $ do
  at 0x1000 $ field "device_type" uint32 & ro
  at 0x1001 $ field "error_register" uint8 & ro
  at 0x1002 $ field "manufacturer_status_register" uint32 & ro
  --at 0x1003 $ field "error_array" (varArray "error_array" 254 uint32)
  -- 0x1004 reserved
  at 0x1005 $ field "sync_cob_id" uint32 & defaultNum 0x80000080
  at 0x1006 $ field "comm_cycle_period" uint32
  at 0x1007 $ field "sync_window_length" uint32
  -- 0x1008 Requires Const Strings (manufacturer_device_name (attr 0x1008 readwrite string_t))
  -- 0x1009 hardware version string_t
  -- 0x100A software version string_t
  -- 0x100B reserved
  -- 0x100C guard time uint16_t
  -- 0x100D life time factor uint8_t
  -- 0x100E reserved
  -- 0x100F reserved
  -- 0x1010 parameter storage, array for storing user parameters in non-volatile storage
  -- 0x1011 restore default parameters, array for restoring parameters
  at 0x1012 $ field "time_cob_id" uint32 & defaultNum 0x100
  at 0x1013 $ field "hr_timestamp" uint32
  at 0x1014 $ field "emcy_cob_id" uint32 & defaultNum 0x80 -- 0x80 + nodeId
  at 0x1015 $ field "emcy_inhibit_time" uint16
  --at 0x1016 $ field "consumer_heartbeat_time" (array "consumer_heartbeat" 128 uint32)
  at 0x1017 $ field "producer_heartbeat_time" uint16
  at 0x1018 $ field "identity" (identity nodeSpec) & ro
  -- 0x1019 to 0x11FF reserved

  -- 0x1200 1st server SDO parameter sdo_commpar_t
  -- 0x1201 2nd server SDO parameter sdo_commpar_t
  -- 0x127F 128th server SDO parameter sdo_commpar_t

  -- 0x1280 1st client SDO parameter sdo_commpar_t
  -- 0x1281 2nd client SDO parameter sdo_commpar_t
  -- 0x12FF 128th client SDO parameter sdo_commpar_t

  -- 0x1300 to 0x13FF reserved
  forM_ [0..1] $ \x -> do
    at (0x1400 + x) $ rpdoCommField $ x + 1
    at (0x1600 + x) $ rpdoMapField $ x + 1

    at (0x1800 + x) $ tpdoCommField $ x + 1
    at (0x1A00 + x) $ tpdoMapField $ x + 1

cia401Dict :: Dict
cia401Dict = dict "cia401" $ do
  at 0x6200
    $ field
        "io_output"
        $ array
            "output"
            1 -- only one register of 8 outputs
            uint8
  -- subindex 0 - number of outputs
  -- subindex 1 - write output 1..8
  -- subindex 1 - write output 9..16

cia402Dict :: Dict
cia402Dict = dict "cia402" $ do
  at 0x6063 $ field "position_actual" sint32

  -- profile velocity
  at 0x6069 $ field "velocity_sensor_actual" sint32 & ro
  at 0x606A $ field "sensor_selection_code" sint16
  at 0x606B $ field "velocity_demand" sint32 & ro
  at 0x606C $ field "velocity_actual" sint32 & ro
  at 0x606D $ field "velocity_window" uint16
  at 0x606E $ field "velocity_window_time" uint16
  at 0x606F $ field "velocity_threshold" uint16
  at 0x6070 $ field "velocity_threshold_time" uint16
  -- profile torque
  at 0x6071 $ field "target_torque" sint16
  at 0x6072 $ field "max_torque" uint16
  at 0x6073 $ field "max_current" uint16
  at 0x6074 $ field "torque_demand" sint16 & ro
  at 0x6075 $ field "motor_rated_current" uint32
  at 0x6076 $ field "motor_rated_torque" uint32
  at 0x6077 $ field "torque_actual" sint16 & ro
  at 0x6078 $ field "current_actual" sint16 & ro
  at 0x6079 $ field "dc_link_circuit_voltage" uint32 & ro

  at 0x6087 $ field "torque_slope" uint32
  at 0x6088 $ field "torque_profile_type" uint16

  -- at 0x60F6 $ field "torque_control_params" -- manufacturer specific record
  -- at 0x60F7 $ field "power_stage_params" -- manufacturer specific record
  at 0x60F8 $ field "max_slippage" sint32
  at 0x60F9 $ field "velocity_paramaters" (array "velocity_params" 2 uint16)
    -- ^^ pid control params
    -- subindex 1 is Gain, subindex 2 is Ti - sintegration time constant
  at 0x60FF $ field "target_velocity" sint32

reflowDict :: Dict
reflowDict = dict "reflow" $ do
  at 0x6000 $ field "temperature" float & ro
  at 0x6001 $ field "temperature_target" float
  at 0x6002 $ field "control_effort" float & ro

  at 0x6100 $ field "pid_p" float
  at 0x6101 $ field "pid_i" float
  at 0x6102 $ field "pid_d" float
  at 0x61FF $ field "pid_reset" bool

  at 0x6200 $ field "pid_p_actual" float & ro
  at 0x6201 $ field "pid_i_actual" float & ro
  at 0x6202 $ field "pid_d_actual" float & ro
  at 0x62FF $ field "pid_output" float & ro

testDict :: Dict
testDict = dict "test" $ do
  at 0x6000 $ field "test_bool" bool
  at 0x6001 $ field "test_u8" uint8
  at 0x6002 $ field "test_u16" uint16
  at 0x6003 $ field "test_u32" uint32
  at 0x6004 $ field "test_s8" sint8
  at 0x6005 $ field "test_s16" sint16
  at 0x6006 $ field "test_s32" sint32
  at 0x6007 $ field "test_float" float
  at 0x6008 $ field "test_double" double

  at 0x606C $ field "test_velocity_actual" sint32 & ro
  at 0x60FF $ field "test_target_velocity" sint32

  at 0x6100 $ field "test_ro" bool & ro
  at 0x6101 $ field "test_constant" bool & constant
  at 0x6102 $ field "test_reserved" bool & reserved
  at 0x6103 $ field "test_default" uint8 & defaultNum 42

  at 0x7000
    $ field
        "test_array"
        (array
          "test_array"
          5
          uint8
        )

  at 0x7001
    $ field
        "test_var_array"
        (varArray
          "test_var_array"
          42
          uint32
        )

  at 0x8000
    $ field "test_record"
    $ record
        "test_record"
        [ field "field1" uint8
        , field "field2" sint8
        ]

--  ConstArray
--  OctetString
--  VisString
--  UnicodeString
--   -- TimeDifference requires bits
--
--  disallow complex types in objdict - arrays of arrays, records of records
--  check for conflicting names/indexes
--  validators
--  feed identity with NodeSpec
--  bitdata
--  reserved values = Abort SDO Transfer (abort code: 0609 0011h).
