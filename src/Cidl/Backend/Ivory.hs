module Cidl.Backend.Ivory where

import Ivory.Artifact
import Ivory.Artifact.Template

import Data.Char (isSpace)
import Data.List (intercalate)

import qualified Paths_cidl as P

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Ivory.Types

ivoryBackend
  :: [Dict]
  -> String
  -> String
  -> [Artifact]
ivoryBackend dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , artifactPath "tests" $ codegenTest namespace
  ] ++ map (artifactPath "src") sources
  where
  sources = ivorySources dicts namespace
  namespace = dotwords namespace_raw

  cf = (defaultCabalFile pkgname cabalmods ivoryDeps) { tests = [ cg_test ] }
  cg_test = defaultCabalTest cg_test_name "CodeGen.hs"
              (ivoryDeps ++ ivoryTestDeps ++ [pkgname])
  cg_test_name = pkgname ++ "-gen"
  cabalmods = map (filePathToPackage . artifactFileName) sources

ivoryDeps :: [String]
ivoryDeps =
  [ "ivory"
  , "ivory-serialize"
  , "ivory-stdlib"
  ]

ivoryTestDeps :: [String]
ivoryTestDeps =
  [ "ivory-backend-c"
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

makefile :: Artifact
makefile =
  artifactCabalFileTemplate P.getDataDir "support/ivory/Makefile.template" []

codegenTest :: [String] -> Artifact
codegenTest modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/ivory/CodeGen.hs.template"
