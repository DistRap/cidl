{-# LANGUAGE RecordWildCards #-}
module Cidl.Backend.EDS where

import Cidl.Dict (Dict(..))
import Cidl.Types.AST (Entry(..), Perm(..), Type(..))
import Cidl.Types.CANOpen (CANOpenType(..))
import Data.Default.Class (Default(def))
import Ivory.Artifact (Artifact)
import Text.Printf (PrintfArg)

import qualified Cidl.Dict
import qualified Cidl.Monad
import qualified Cidl.Types.Base
import qualified Cidl.Types.CANOpen
import qualified Cidl.Utils
import qualified Data.List
import qualified Ivory.Artifact
import qualified Text.Printf

edsBackend :: [Dict] -> String -> String -> [Artifact]
edsBackend dicts _pkgname _namespace_raw =
  [ Ivory.Artifact.artifactString
      (dictName d ++ ".eds")
      (buildDict d)
    | d <- dicts
  ]
  where
  buildDict d =
    renderIni
    $ Data.List.intersperse
        IniBlank
    $ (preamble++)
    $ concatMap
        buildEntries
    $ concatMap
        Cidl.Dict.allEntries
        [ (Cidl.Monad.commsDict def)
        , d
        ]

  buildEntries e@Entry{..} =
    let
      -- object indices are w/o 0x
      strIdx = drop 2 $ fmtHex entryIndex
    in
    case entryTyp of
      RecordType _recordName subEntries ->
        [ addSubNumber
            (length subEntries + 1) -- +1 because of count entry at sub0
            $ buildEntry
                strIdx
                e
        -- first entry of a record is its Uint8 count of sub-indices
        , buildEntry
              (strIdx <> "sub0")
              e { entryTyp = Cidl.Types.Base.uint8
                , entryName = entryName <> "_count" }
        ]
        ++
        map
          (\(subE, subIdx) ->
            buildEntry
              (strIdx <> "sub" <> show subIdx)
              subE
          )
          (zip subEntries [(1 :: Int) ..])
      ArrayType _arrName arrLen arrTyp ->
        [ addSubNumber
            (arrLen + 1) -- +1 because of count entry at sub0
            $ buildEntry
                strIdx
                e
        -- first entry of an array is its Uint8 count of sub-indices
        , buildEntry
              (strIdx <> "sub0")
              e { entryTyp = Cidl.Types.Base.uint8
                , entryName = entryName <> "_count" }
        ]
        ++
        map
          (\subIdx ->
            buildEntry
              (strIdx <> "sub" <> show subIdx)
              e { entryTyp = arrTyp
                , entryName = entryName <> "_" <> show subIdx }
          )
          [(1 :: Int) .. arrLen]

      -- TODO: ignore these for now as they are no good
      -- VarArrayType subRecordType -> buildEntries (e { entryTyp = subRecordType })
      VarArrayType _subRecordType -> mempty
      PrimType _primType -> pure $ buildEntry strIdx e

  buildEntry idx Entry{..} =
    let
      CANOpenType{..} = Cidl.Types.CANOpen.toCANOpenType entryTyp
    in
    IniSection
     idx
     $ [ ("ParameterName"
         , Cidl.Utils.firstCap
           $ Cidl.Utils.snakeToCamel
           $ entryName
         )
       , ("ObjectType"
         , fmtHex $ fromEnum $ canOpenTypeObjectType
         )
       , ("AccessType"
         , buildPerm entryAccess
         )
       , ("PDOMapping"
         , buildBool entryPdoMappable)
       ]
       ++ maybe
            mempty
            (\dt ->
               pure ("DataType"
                    , fmtHex $ fromEnum $ dt
                    )
            )
            canOpenTypeDataType

  addSubNumber
    :: Int
    -> Ini
    -> Ini
  addSubNumber subNum is@IniSection{..} =
    is
    { iniSectionValues =
        iniSectionValues
        ++ pure ("Subentries", (show subNum))
    }
  addSubNumber _ is = is

  buildBool True = "1"
  buildBool False = "0"

  buildPerm Read = "ro"
  buildPerm Write = "wo"
  buildPerm ReadWrite = "rw"
  buildPerm Const = "const"
  buildPerm Reserved = buildPerm Const

fmtHex
  :: PrintfArg t
  => t
  -> String
fmtHex =
  Text.Printf.printf
    "0x%04x"

data Ini =
    IniSection
    { iniSectionName :: String
    , iniSectionValues :: [(String, String)]
    }
  | IniBlank
  | IniComment String
  deriving (Eq, Show)

renderIni :: [Ini] -> String
renderIni = unlines . concatMap go
  where
    go IniBlank = pure mempty
    go (IniComment x) = pure $ "; " <> x
    go IniSection{..} =
      ("[" <> iniSectionName <> "]")
      : map
         (\(key, val) -> key <> "=" <> val)
         iniSectionValues

preamble :: [Ini]
preamble =
  [ IniSection
      "DeviceInfo"
      $ [ ("VendorName", "DistRap")
        , ("VendorNumber", "42")
        , ("ProductName", "Demo")
        , ("ProductNumber", "13")
        , ("RevisionNumber", "37")
        , ("OrderCode", "13-37")
        , ("SimpleBootUpMaster", "0")
        , ("SimpleBootUpSlave" , "0")
        , ("Granularity", "8")
        , ("DynamicChannelsSupported", "0")
        , ("CompactPDO", "0")
        , ("GroupMessaging", "0")
        , ("NrOfRXPDO", "2")
        , ("NrOfTXPDO", "2")
        , ("LSS_Supported", "1")
        ]
        ++ map
            (\b -> ("BaudRate_" <> show (b :: Int), "1"))
            -- pretend, we do not really support 800K IIRC
            [10, 20, 50, 125, 250, 500, 800, 1000]
  , IniSection
      "MandatoryObjects"
      $ [ ("SupportedObjects", "3")
        , ("1", "0x1000")
        , ("2", "0x1001")
        , ("3", "0x1018")
        ]
  ]
