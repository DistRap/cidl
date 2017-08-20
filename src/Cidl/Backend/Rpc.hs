module Cidl.Backend.Rpc (
    rpcBackend
  ) where

import qualified Paths_cidl as P

import Cidl.Backend.Cabal (cabalFileArtifact,defaultCabalFile,filePathToPackage)
import Cidl.Backend.Haskell.Dict (interfaceModule,ifModuleName)
import Cidl.Backend.Haskell.Types
           (typeModule,isUserDefined,typeModuleName,userTypeModuleName
           ,importType,importDecl, qualifiedImportDecl)
--import Cidl.Interface
--           (Interface(..),MethodName,Method(..)
--           ,interfaceMethods)
--import Cidl.Schema
--           (Schema(..),producerSchema,consumerSchema,Message(..)
--           ,consumerMessages,interfaceTypes,getResponseMessage)
import Cidl.Types (Type(..), Perm(..))

import Data.Char (isSpace)
import Data.List (nub)
import Ivory.Artifact
           (Artifact,artifactPath,artifactFileName,artifactPath,artifactText
           ,artifactCabalFile)
import Ivory.Artifact.Template (artifactCabalFileTemplate)
import Text.PrettyPrint.Mainland
           (Doc,prettyLazyText,text,empty,(<+>),(</>),(<>),char,line,parens
           ,punctuate,stack,tuple,dot,spread,cat,hang,nest,align,comma
           ,braces,brackets,dquotes)


-- External Interface ----------------------------------------------------------

rpcBackend :: [Interface] -> String -> String -> [Artifact]
rpcBackend iis pkgName nsStr =
  [ cabalFileArtifact (defaultCabalFile pkgName modules buildDeps)
  , artifactCabalFile P.getDataDir "support/rpc/Makefile"
  , stackfile
  ] ++  map (artifactPath "src") sourceMods

  where

  namespace  = strToNs nsStr

  buildDeps  = [ "cereal", "QuickCheck", "snap-core", "snap-server", "stm"
               , "aeson", "transformers", "containers", "bytestring", "time"
               , "time-locale-compat" ]

  modules    = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]

  sourceMods = tmods ++ imods ++ [rpcBaseModule namespace]

  types      = nub [ t | i <- iis, t <- interfaceTypes i]
  tmods      = [ typeModule True (namespace ++ ["Types"]) t
               | t <- types
               , isUserDefined t
               ]

  imods      = concat [ [ interfaceModule True (namespace ++ ["Interface"]) i
                        , rpcModule namespace i ]
                      | i <- iis
                      ]
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



rpcBaseModule :: [String] -> Artifact
rpcBaseModule ns =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Rpc" ns) $
  artifactCabalFileTemplate P.getDataDir "support/rpc/Base.hs.template" env
  where
  env = [ ("module_path", foldr (\p rest -> p ++ "." ++ rest) "Rpc" ns) ]


-- Utilities -------------------------------------------------------------------

strToNs :: String -> [String]
strToNs str =
  case break (== '.') (dropWhile isSpace str) of

    (a,'.' : b) | null a    ->          strToNs b
                | otherwise -> trim a : strToNs b

    (a,_)       | null a    -> []
                | otherwise -> [trim a]

  where
  trim = takeWhile (not . isSpace)


allMethods :: Interface -> [(MethodName,Method)]
allMethods (Interface _ ps ms) = concatMap allMethods ps ++ ms


isEmptySchema :: Schema -> Bool
isEmptySchema (Schema _ ms) = null ms


-- Server Generation -----------------------------------------------------------

rpcModule :: [String] -> Dict -> Artifact
rpcModule ns iface =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Rpc" ns) $
  artifactText (ifaceMod ++ ".hs") $
  prettyLazyText 1000 $
  genServer ns iface ifaceMod
  where
  ifaceMod = ifModuleName iface


genServer :: [String] -> Interface -> String -> Doc
genServer ns iface ifaceMod = stack $
  [ text "{-# LANGUAGE RecordWildCards #-}" | useManager ] ++
  [ text "{-# LANGUAGE OverloadedStrings #-}"
  , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
  , moduleHeader     ns ifaceMod
  , line
  , importTypes      ns iface
  , importInterface  ns ifaceMod
  , line
  , text "import" <+> (ppModName (ns ++ ["Rpc","Base"]))
  ] ++
  [ line
  , webServerImports hasConsumer
  , line
  , line
  , runServer hasConsumer useManager iface input output
  ]
  where
  hasConsumer = not (isEmptySchema (consumerSchema iface))
  useManager = False

  (input,output) = queueTypes iface


moduleHeader :: [String] -> String -> Doc
moduleHeader ns m =
  spread [ text "module"
         , dots (map text (ns ++ ["Rpc", m]))
         , tuple [ text "rpcServer", text "Config(..)" ]
         , text "where"
         ]


-- | Import the type modules required by the interface.  Import hiding
-- everything, as we just need the ToJSON/FromJSON instances.
importTypes :: [String] -> Interface -> Doc
importTypes ns iface = stack
                     $ map (streamImport . importType) streams
                    ++ map (typeImport   . importType) types
  where
  (streams,itypes) = partitionTypes iface

  types = itypes ++ interfaceTypes iface

  streamImport ty = importDecl addNs ty
  typeImport   ty = qualifiedImportDecl addNs ty

  prefix  = dots (map text (ns ++ ["Types"]))
  addNs m = prefix <> char '.' <> text m


-- | Separate the types that are used from a stream method, from those used
-- in attribute methods.
partitionTypes :: Interface -> ([Type],[Type])
partitionTypes iface = go [] [] (interfaceMethods iface)
  where
  go s a []                           = (nub s, nub a) 
  go s a ((_,AttrMethod addr _ ty):rest) = go     s  (ty:a) rest


importInterface :: [String] -> String -> Doc
importInterface ns ifaceName =
  text "import" <+> (dots (map text (ns ++ ["Interface", ifaceName])))


webServerImports :: Bool -> Doc
webServerImports hasConsumer = stack $
  [ text "import           Control.Monad (msum)" | hasConsumer ] ++
  [ text "import           Data.Aeson (decode)"  | hasConsumer ] ++
  [ text "import qualified Snap.Core as Snap"
  , text "import           Control.Concurrent (forkIO)"
  , text "import           Control.Concurrent.STM"
  , text "import           Control.Monad (forever)"
  , text "import           Control.Monad.IO.Class (liftIO)"
  , text "import           Data.Aeson (encode,Value(Null))"
  ]


type InputQueue  = Doc
type OutputQueue = Doc

queueTypes :: Interface -> (InputQueue,OutputQueue)
queueTypes iface = (input,output)
  where
  Schema prodName _ = producerSchema iface
  Schema consName _ = consumerSchema iface

  prod = ifModuleName iface ++ prodName
  cons = ifModuleName iface ++ consName

  input  = text "TQueue" <+> text prod
  output = text "TQueue" <+> text cons


runServer :: Bool -> Bool -> Interface -> InputQueue -> OutputQueue -> Doc
runServer hasConsumer useMgr iface input output =
  runServerSig hasConsumer input output </>
  runServerDef hasConsumer useMgr iface


runServerSig :: Bool -> InputQueue -> OutputQueue -> Doc
runServerSig hasConsumer input output =
  text "rpcServer ::" <+> hang 2 (arrow tys)

  where
  tys = [ input                       ] ++
        [ output | hasConsumer        ] ++
        [ text "Config", text "IO ()" ]

-- | Generate a definition for the server.
runServerDef :: Bool -> Bool -> Interface -> Doc
runServerDef hasConsumer useMgr iface =
  hang 2 (text "rpcServer" <+> body)
  where
  args = spread $
    [ text "input"                ] ++
    [ text "output" | hasConsumer ] ++
    [ text "cfg"                  ]

  body =  args <+> char '=' </> nest 2 (doStmts stmts)

  stmts = [ text "state <- mkState"                         | useMgr      ]
       ++ [ defInput                                                      ]
       ++ [ spread $ [ text "_ <- forkIO (manager state input" ]
                  ++ [ text "input'" | hasConsumer ]
                  ++ [ text ")" ]                           | useMgr      ]
       ++ [ text "conn <- newConn output" <+> input'
                  <+> seqNumGetter                          | hasConsumer ]
       ++ [ text "logCtx <- initLogging (cfgLogSuffix cfg)"               ]
       ++ [ text "runServer cfg $ Snap.route" </> routesDef               ]

  (input',defInput)
    | hasConsumer && useMgr = (text "input'", text "input' <- newTQueueIO")
    | otherwise             = (text "input", empty)

  routesDef = nest 2 (align (routes iface (text "state")))

  seqNumGetter = parens (text "SequenceNum.unSequenceNum ."
      <+> text "seqNumGetter" <> text (ifModuleName iface) <> text prodName)
  Schema prodName _ = producerSchema iface

-- | Define one route for each interface member
routes :: Interface -> Doc -> Doc
routes iface state =
  align (char '[' <> nest 1 (stack (commas handlers)) <> char ']')
  where
  Interface pfx _ _ = iface
  Schema suffix _   = consumerSchema iface

  handlers = map (mkRoute pfx suffix state) (allMethods iface)


mkRoute :: String -> String -> Doc -> (MethodName,Method) -> Doc
mkRoute ifacePfx consSuffix state method@(name,mty) =
  parens (url <> comma </> guardMethods (handlersFor mty))
  where
  url = dquotes (text ifacePfx <> char '/' <> text name)

  guardMethods [h] = h
  guardMethods hs  = nest 2 $ text "msum"
                          </> brackets (stack (commas hs))

  handlersFor (AttrMethod addr Read _) =
      [ readAttr method consSuffix m | (addr', m) <- consumerMessages method ]

  handlersFor (AttrMethod addr Write _) =
      [ writeAttr consSuffix m | (addr', m) <- consumerMessages method ]

  handlersFor (AttrMethod addr ReadWrite ty) =
      [ readAttr method consSuffix m
      | (addr', m) <- consumerMessages (name,AttrMethod addr Read ty) ] ++
      [ writeAttr consSuffix m | (_, m) <- consumerMessages (name,AttrMethod addr Write ty) ]


readStream :: Doc -> MethodName -> Doc
readStream state name = nest 2 $ text "Snap.method Snap.GET $"
  </> doStmts
    [ text "x <- liftIO (atomically (readTSampleVar" <+> svar <> text "))"
    , text "let e = case x of Just v -> encode v; Nothing -> encode Null"
    , text "writeLoggingLBS logCtx e"
    ]
  where
  svar = parens (fieldName name <+> state)

constrName :: String -> Message -> String
constrName suffix (Message n _) = userTypeModuleName n ++ suffix

readAttr :: (MethodName,Method) -> String -> Message -> Doc
readAttr (attrname, (AttrMethod addr _ t)) suffix msg =
  text "Snap.method Snap.GET $" <+> doStmts
    [ parens (text responseConstructor
             <+> parens (responseSNumed <> dot <> responseSNumed
                        <+> text "_ resp"))
      <+> text "<- liftIO $ sendRequest conn $"
                     <+> text (constrName suffix msg)
                     <+> dot <+> text "SequenceNum.SequenceNum"

    , text "Snap.modifyResponse $ Snap.setContentType \"application/json\""
    , text "writeLoggingLBS logCtx (encode resp)"
    ]
  where
  resp@(Message _ (RecordType resp_tyname _)) = getResponseMessage attrname t
  responseConstructor = constrName "Producer" resp
  responseSNumed = text $ userTypeModuleName resp_tyname

readAttr _ _ _ = error "impossible readAttr"

writeAttr :: String -> Message -> Doc
writeAttr suffix msg = text "Snap.method Snap.POST $" <+> doStmts
  [ text "bytes <- Snap.readRequestBody 32768"
  , text "case decode bytes of" </>
      text "Just req -> liftIO $" <+> doStmts
        [ text "_ <- sendRequest conn $ \\ snum ->"
                    <+> text con
                    <+> parens (text (userTypeModuleName sname)
                                <> dot <> text (userTypeModuleName sname)
                                <+> text "(SequenceNum.SequenceNum snum)" <+> text "req")
        , text "return ()"
        ] </>
      text "Nothing  -> Snap.modifyResponse $ Snap.setResponseCode 400"
  ]
  where
  con = constrName suffix msg
  (Message _ (RecordType sname _)) = msg


-- | Given the name of a stream in the interface, produce the selector for the
-- state data type.
fieldName :: MethodName -> Doc
fieldName name = text "stream_" <> text name


-- Pretty-printing Helpers -----------------------------------------------------

arrow :: [Doc] -> Doc
arrow ts = spread (punctuate (text "->") ts)

commas :: [Doc] -> [Doc]
commas  = punctuate comma

dots :: [Doc] -> Doc
dots  = cat . punctuate dot

ppModName :: [String] -> Doc
ppModName  = dots . map text

doStmts :: [Doc] -> Doc
doStmts [d] = nest 2 d
doStmts ds  = text "do" <+> align (stack (map (nest 2) ds))
