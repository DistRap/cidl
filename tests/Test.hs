module Main where

import Ivory.Artifact
import Control.Monad
import Cidl.Types
import Cidl.Interface
import Cidl.Parse
import Cidl.Schema
import Cidl.Backend.Cabal
import Cidl.Backend.Haskell.Types
import Cidl.Backend.Haskell.Interface
import Cidl.Backend.Haskell

main :: IO ()
main = do
  test "tests/testtypes.sexpr"
  runHaskellBackend "tests/testtypes.sexpr"
                    "cidl-haskell-backend-test"
                    (words "Cidl Haskell Test")
                    "tests/cidl-haskell-backend-test"


test :: FilePath -> IO ()
test f = do
  c <- readFile f
  case parseDecls c of
    Left e -> putStrLn e
    Right (te@(TypeEnv te'), ie@(InterfaceEnv ie')) -> do
      {-
      forM_ te' $ \(tn, t) -> do
        putStrLn (tn ++ ":")
        print (typeLeaves t)
        let a = typeModule (words "Sample IDL Haskell Types")
                           (typeDescrToRepr tn te)
        printArtifact a
      -}
      forM_ ie' $ \(iname, _i) -> do
        printArtifact $ interfaceModule (words "Sample IDL Haskell Interface")
                                        (interfaceDescrToRepr iname ie te)
