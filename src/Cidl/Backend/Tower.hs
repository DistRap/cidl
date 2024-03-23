module Cidl.Backend.Tower where

import Data.List (nub)

import Ivory.Artifact
import Ivory.Artifact.Template

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Ivory (dotwords, ivorySources)
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
    ]
