module Cidl.Backend.Haskell where

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Haskell.Types
import Cidl.Backend.Haskell.Dict

import Ivory.Artifact

import Data.Char (isSpace)
import Text.PrettyPrint.Mainland

haskellBackend :: [Dict] -> String -> String -> [Artifact]
haskellBackend dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  userDefinedTypes = [ t | d <- dicts, t <- allTypes d, isUserDefined t ]
  tmods = [ typeModule False (namespace ++ ["Types"]) t
          | t <- userDefinedTypes
          ]
  imods = [ interfaceModule (namespace ++ ["Interface"]) d
          | d <- dicts
          ]
  sourceMods = tmods ++ imods
  cf = defaultCabalFile pkgname cabalmods deps
  cabalmods = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]
  deps =
    [ "cereal"
    , "QuickCheck"
    , "network-canopen"
    ]

  namespace = dotwords namespace_raw

  dotwords :: String -> [String]
  dotwords s = case dropWhile isDot s of
    "" -> []
    s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  isDot c = (c == '.') || isSpace c

makefile :: Artifact
makefile = artifactText "Makefile" $
  prettyLazyText 1000 $ stack
    [ text "default:"
    , text "\tcabal build ."
    , text "clean:"
    , text "\tcabal clean"
    , text "test:"
    , text "\tcabal test ."
    ]
