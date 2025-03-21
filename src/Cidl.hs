module Cidl
  ( run
  , runCidl
  , module Cidl.Monad
  , module Cidl.Types.AST
  , module Cidl.Types.Base
  ) where

import Prelude

import Data.Char
import Data.Maybe (catMaybes)
import Control.Monad (when)
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Show.Pretty

import Ivory.Artifact
import Cidl.Dict
import Cidl.Monad
import Cidl.Types.AST
import Cidl.Types.Base
import Cidl.Backend.EDS
import Cidl.Backend.Haskell
import Cidl.Backend.Tower

data OptParser opt = OptParser [String] (opt -> opt)
instance Semigroup (OptParser opt) where
  OptParser as f <> OptParser bs g = OptParser (as ++ bs) (f . g)

instance Monoid (OptParser opt) where
  mempty = OptParser [] id

success :: (opt -> opt) -> OptParser opt
success  = OptParser []

invalid :: String -> OptParser opt
invalid e = OptParser [e] id

parseOptions :: [OptDescr (OptParser opt)] -> [String]
             -> Either [String] (opt -> opt)
parseOptions opts args = case getOpt Permute opts args of
  (fs,[],[]) -> case mconcat fs of
    OptParser [] f -> Right f
    OptParser es _ -> Left es
  (_,_,es) -> Left es

data Backend
  = EDSBackend
  | HaskellBackend
  | TowerBackend
  deriving (Eq, Show)

data Opts = Opts
  { backend             :: Backend
  , outpath             :: FilePath
  , packagename         :: String
  , namespace           :: String
  , debug               :: Bool
  , help                :: Bool
  }

initialOpts :: Opts
initialOpts = Opts
  { backend             = error (usage ["must specify a backend"])
  , outpath             = error (usage ["must specify an output path"])
  , packagename         = error (usage ["must specify a package name"])
  , namespace           = ""
  , debug               = False
  , help                = False
  }

setBackend :: String -> OptParser Opts
setBackend b = case map toUpper b of
  "EDS" -> success (\o -> o { backend = EDSBackend })
  "HASKELL" -> success (\o -> o { backend = HaskellBackend })
  "TOWER" -> success (\o -> o { backend = TowerBackend })
  _ -> invalid e
  where e = "\"" ++ b ++ "\" is not a valid backend.\n"
          ++ "Supported backends: haskell, tower"

setOutPath :: String -> OptParser Opts
setOutPath p = success (\o -> o { outpath = p })

setPackageName :: String -> OptParser Opts
setPackageName p = success (\o -> o { packagename = p })

setNamespace :: String -> OptParser Opts
setNamespace p = success (\o -> o { namespace = p })

setDebug :: OptParser Opts
setDebug = success (\o -> o { debug = True })

setHelp :: OptParser Opts
setHelp = success (\o -> o { help = True })

options :: [OptDescr (OptParser Opts)]
options =
  [ Option "b" ["backend"]   (ReqArg setBackend "BACKEND")
      "code generator backend"
  , Option "o" ["out"]       (ReqArg setOutPath "DIR")
      "root directory for output"
  , Option "p" ["package"]   (ReqArg setPackageName "NAME")
      "package name for output"
  , Option "n" ["namespace"] (ReqArg setNamespace "NAME")
      "namespace for output"
  , Option ""  ["debug"]     (NoArg setDebug)
      "enable debugging output"
  , Option "h" ["help"]      (NoArg setHelp)
      "display this message and exit"
  ]

parseOpts :: [String] -> IO Opts
parseOpts args = case parseOptions options args of
  Right f -> let opts = f initialOpts in
    if help opts then putStrLn (usage []) >> exitSuccess
                 else return opts
  Left errs -> putStrLn (usage errs) >> exitFailure


usage :: [String] -> String
usage errs = usageInfo banner options
  where
  banner = unlines (errs ++ ["", "Usage: cidl OPTIONS"])

run :: IO ()
run = runCidl interfaces

runCidl :: [Dict] -> IO ()
runCidl dicts = do
  args <- getArgs
  opts <- parseOpts args
  when (debug opts) $ do
    putStrLn (ppShow dicts)

  b <- case backend opts of
         EDSBackend -> pure edsBackend
         HaskellBackend -> pure haskellBackend
         TowerBackend -> pure towerBackend
  artifactBackend opts (b dicts (packagename opts) (namespace opts))

  where
  artifactBackend :: Opts -> [Artifact] -> IO ()
  artifactBackend opts as = do
    when (debug opts) $ mapM_ printArtifact as
    es <- mapM (putArtifact (outpath opts)) as
    case catMaybes es of
      [] -> exitSuccess
      ees -> putStrLn (unlines ees) >> exitFailure
