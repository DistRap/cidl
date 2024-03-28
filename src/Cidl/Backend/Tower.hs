module Cidl.Backend.Tower where

import Data.Char (isSpace)
import Data.List (nub)
import Text.PrettyPrint.Mainland

import Ivory.Artifact

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Ivory.Types
import Cidl.Backend.Tower.Dict

towerBackend
  :: [Dict]
  -> String
  -> String
  -> [Artifact]
towerBackend dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , artifactString
     "cabal.project"
     "packages: ."
  , makefile
  ] ++ map (artifactPath "src") sources
  where
  namespace = dotwords namespace_raw

  sources = isources ++ tsources

  tsources = towerSources dicts (namespace ++ ["Tower"])

  isources = ivorySources dicts (namespace ++ ["Ivory"])

  cf = defaultCabalFile pkgname cabalmods towerDeps
  cabalmods = nub $ map (filePathToPackage . artifactFileName) sources

towerDeps :: [String]
towerDeps =
  [ "ivory"
  , "ivory-serialize"
  , "ivory-stdlib"
  , "tower"
  , "ivory-tower-canopen-core"
  ]

towerTestDeps :: [String]
towerTestDeps =
  [ "tower-config"
  , "tower-freertos-stm32"
  ]

towerSources :: [Dict] -> [String] -> [Artifact]
towerSources dicts namespace = towerInterfaces
  where
  towerInterfaces = concat
    [ [ dictModule    ifnamespace d
      , umbrellaModule  ifnamespace d
      ]
    | d <- dicts ]
  ifnamespace = namespace ++ ["Interface"]

makefile :: Artifact
makefile = artifactText "Makefile" $
  prettyLazyText 1000 $ stack
    [ text "default:"
    , text "\tcabal build"
    , text "clean:"
    , text "\tcabal clean"
    ]

ivorySources :: [Dict] -> [String] -> [Artifact]
ivorySources dicts namespace =
  tmods ++ [ typeUmbrella namespace userDefinedTypes ]
  where
  userDefinedTypes = [ t | d <- dicts, t <- allTypes d, isUserDefined t ]
  tmods = [ typeModule (namespace ++ ["Types"]) t
          | t <- userDefinedTypes ]

dotwords :: String -> [String]
dotwords s = case dropWhile isDot s of
  "" -> []
  s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  where
  isDot c = (c == '.') || isSpace c
