module Cidl.Backend.Tower where

import Data.List (nub)
import Text.PrettyPrint.Mainland

import Ivory.Artifact
import Ivory.Artifact.Template

import qualified Paths_cidl as P

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Ivory (dotwords, ivorySources)
import Cidl.Backend.Tower.Dict

towerBackend :: FilePath -> FilePath -> FilePath
             -> [Dict] -> String -> String -> [Artifact]
towerBackend ivoryRepo towerRepo ivoryTowerSTM32Repo dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , stackfile ivoryRepo towerRepo ivoryTowerSTM32Repo
  , defaultconf
--  , artifactPath "tests" (codegenTest iis namespace)
  ] ++ map (artifactPath "src") sources
  where
  namespace = dotwords namespace_raw

  sources = isources ++ tsources ++ [attrs, dicttypes]

  tsources = towerSources dicts (namespace ++ ["Tower"])

  isources = ivorySources dicts (namespace ++ ["Ivory"])

  cf = (defaultCabalFile pkgname cabalmods towerDeps)
  cabalmods = nub $ map (filePathToPackage . artifactFileName) sources

towerDeps :: [String]
towerDeps =
  [ "ivory"
  , "ivory-serialize"
  , "ivory-stdlib"
  , "tower"
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
makefile =
  artifactCabalFileTemplate P.getDataDir "support/tower/Makefile.template" []

stackfile :: FilePath -> FilePath -> FilePath -> Artifact
stackfile ivoryRepo towerRepo ivoryTowerSTM32Repo = artifactText "stack.yaml" $
  prettyLazyText 1000 $ stack
    [ text "resolver: lts-9.1"
    , empty
    , text "packages:"
    , text "- '.'"
    , text ("- location: " ++ ivoryRepo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory"
    , text "    - ivory-artifact"
    , text "    - ivory-backend-c"
    , text "    - ivory-hw"
    , text "    - ivory-opts"
    , text "    - ivory-serialize"
    , text "    - ivory-stdlib"
    , text ("- location: " ++ towerRepo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - tower"
    , text "    - tower-config"
    , text "    - tower-hal"
    , text ("- location: " ++ ivoryTowerSTM32Repo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory-bsp-stm32"
    , text "    - ivory-freertos-bindings"
    , text "    - tower-freertos-stm32"
    , text "  extra-dep: true"
    , empty
    , text "install-ghc: true"
    , empty
    , text "extra-deps:"
    , text "- monadLib-3.7.3"
    , empty
    ]

defaultconf :: Artifact
defaultconf = artifactCabalFile P.getDataDir "support/tower/default.conf"

attrs :: Artifact
attrs = artifactPath "CANOpen/Tower" $
  artifactCabalFile P.getDataDir "support/tower/Attr.hs"

dicttypes :: Artifact
dicttypes = artifactPath "CANOpen/Tower" $
  artifactCabalFile P.getDataDir "support/tower/Types.hs"
