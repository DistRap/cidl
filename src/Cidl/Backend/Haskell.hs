module Cidl.Backend.Haskell where

import Cidl.Dict
import Cidl.Backend.Cabal
import Cidl.Backend.Haskell.Types
import Cidl.Backend.Haskell.Dict

import Ivory.Artifact

import Data.Char (isSpace)
import Data.List (nub)
import Text.PrettyPrint.Mainland
import Lens.Family2

haskellBackend :: [Dict] -> String -> String -> [Artifact]
haskellBackend dicts pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , stackfile
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  types = nub [ t | d <- dicts, t <- (d ^. dictTypes)]
  tmods = [ typeModule False (namespace ++ ["Types"]) t
          | t <- types
          , isUserDefined t
          ]
  imods = [ interfaceModule False (namespace ++ ["Interface"]) d
          | d <- dicts
          ]
  sourceMods = tmods ++ imods
  cf = defaultCabalFile pkgname cabalmods deps
  cabalmods = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]
  deps = [ "cereal", "QuickCheck" ]

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
    , text "\tstack --stack-yaml stack.yaml build ."
    , empty
    , text "test:"
    , text "\tstack --stack-yaml stack.yaml test ."
    , empty
    ]

stackfile :: Artifact
stackfile = artifactText "stack.yaml" $
  prettyLazyText 1000 $ stack
    [ text "resolver: lts-9.1"
    , empty
    , text "packages:"
    , text "- '.'"
    , empty
    , text "install-ghc: true"
    , empty
    ]
