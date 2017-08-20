module Cidl.Backend.Ivory where

import Ivory.Artifact
import Ivory.Artifact.Template

import Data.Char (isSpace)
import Data.List (intercalate)
import Text.PrettyPrint.Mainland

import qualified Paths_cidl as P

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Ivory.Types

ivoryBackend :: FilePath -> [Dict] -> String -> String -> [Artifact]
ivoryBackend ivoryRepo dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , stackfile ivoryRepo
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

stackfile :: FilePath -> Artifact
stackfile ivory = artifactText "stack.yaml" $
  prettyLazyText 1000 $ stack
    [ text "resolver: lts-9.1"
    , empty
    , text "packages:"
    , text "- '.'"
    , text ("- location: " ++ ivory)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory"
    , text "    - ivory-artifact"
    , text "    - ivory-backend-c"
    , text "    - ivory-opts"
    , text "    - ivory-serialize"
    , text "    - ivory-stdlib"
    , empty
    , text "extra-deps:"
    , text "  - exception-mtl-0.4"
    , text "  - ghc-srcspan-plugin-0.2.1.0"
    , text "  - language-c-quote-0.11.6"
    , text "  - mainland-pretty-0.4.1.2"
    , text "  - symbol-0.2.4"
    , empty
    , text "install-ghc: true"
    , empty
    ]


codegenTest :: [String] -> Artifact
codegenTest modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/ivory/CodeGen.hs.template"
