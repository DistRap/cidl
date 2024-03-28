
module Cidl.Backend.Tower.Dict where

import Control.Lens ((^.))
import Data.List (intercalate, nub, intersperse)

import Cidl.Dict
import Cidl.Lens
import Cidl.Types
import Cidl.Utils
import Cidl.Backend.Ivory.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland
import Text.Printf

fmtHex :: PrintfArg t => t -> Doc
fmtHex n = text $ printf "0x%04x" n

umbrellaModule :: [String] -> Dict -> Artifact
umbrellaModule modulepath d =
  artifactPath (intercalate "/" modulepath) $
  artifactText (dictModuleName d ++ ".hs") $
  prettyLazyText 1000 $
  stack
    [ text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , text "{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}"
    , text "module" <+> mname
    , indent 2 $ encloseStack lparen (rparen <+> text "where") comma
        [
          text "module" <+> im "Dict"
        ]
    , text "import" <+> im "Dict"
    ]
  where
  modAt path = mconcat (punctuate dot (map text path))
  mname = modAt (modulepath ++ [dictModuleName d])
  im m = modAt (modulepath ++ [dictModuleName d, m])

dictModule :: [String] -> Dict -> Artifact
dictModule modulepath d =
  artifactPath (intercalate "/" (modulepath ++ [dictModuleName d])) $
  artifactText "Dict.hs" $
  prettyLazyText 1000 $
  stack
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE RankNTypes #-}"
    , text "{-# LANGUAGE ScopedTypeVariables #-}"
    , text "{-# LANGUAGE KindSignatures #-}"
    , text "{-# LANGUAGE RecordWildCards #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-matches #-}"
    , empty
    , text "module"
      <+> im "Dict"
      <+> text "where"
    , empty
    , stack imports
    , empty
    , attrsDataType d
    , empty
    , attrsTowerConstructor d
    , empty
    , attrsInitializer d
    , empty
    , objdictTower d
    ]
  where
  rootpath = reverse . drop 2 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [dictModuleName d, mname])
  tm mname = modAt (rootpath modulepath ++ ["Ivory","Types", mname])

  imports =
    [ text "import Data.Kind (Type)"
    , text "import Ivory.Language"
    , text "import Ivory.Stdlib"
    , text "import Ivory.Tower"
    , text "import Ivory.Serialize.LittleEndian"
    , text "import CANOpen.Tower.Attr"
    , text "import CANOpen.Tower.Types"
    ] ++ typeimports

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ allTypes d

attrsDataType :: Dict -> Doc
attrsDataType d = text "data" <+> constructor <+> text "(p :: Area Type -> Type) ="
               </> indent 2 constructor
               </> indent 4 body
  where
  constructor = text (dictModuleName d) <> text "Attrs"
  body = encloseStack lbrace rbrace comma $
    [ text n <+> colon <> colon <+> text "p"
                 <+> typeIvoryArea Embedded (e ^. typ)
    | e <- allEntries d
    , let n = userEnumValueName (e ^. name)
    ]
    ++
    [ text n <+> colon <> colon
      <+> lbracket
      <> parens (
        text "p" <+> typeIvoryArea Embedded (ptyp ^. typ)
        <+> comma <+> text "p" <+> typeIvoryArea Embedded (mtyp ^. typ)
        )
      <> rbracket
    | n <- ["rpdos", "tpdos"]
    , let (ptyp, mtyp) = head $ pdos d
    , not $ null $ pdos d
    ]

attrsTowerConstructor :: Dict -> Doc
attrsTowerConstructor d = typesig </> decl </> indent 2 body </> indent 2 ret
  where
  constructor = text (dictModuleName d) <> text "Attrs"
  typesig = text "tower" <> constructor <+> colon <> colon
    <+> constructor <+> text "Init"
    <+> text "->"
    <+> text "Tower e" <+> parens (constructor <+> text "Attr")
  decl = text "tower" <> constructor <+> text "ivals = do"
  body = stack
    [ text n <> text "_p <- towerAttr"
       <+> dquotes (text (e ^. name ++ "_attr"))
       <+> parens (text n <+> text "ivals")
    | e <- allEntries d
    , let n = userEnumValueName (e ^. name)
    ]
  ret = text "pure" <+> constructor <+> (encloseStack lbrace rbrace comma $
    [ text n <+> equals <+> text n <> text "_p"
    | e <- allEntries d
    , let n = userEnumValueName (e ^. name)
    ]
    ++
    [ text n <+> equals
      <+> encloseStack lbracket rbracket comma
            ([ parens (
                text (userEnumValueName (eParam ^. name)) <> text "_p"
                <+> comma <+>
                text (userEnumValueName (eMap ^. name)) <> text "_p"
                )
            | (eParam, eMap) <- es
            ])
    | (n, es) <- [("rpdos", rpdos d), ("tpdos", tpdos d)]
    , not $ null $ pdos d
    ])

attrsInitializer :: Dict -> Doc
attrsInitializer d = typesig </> decl </> indent 2 body
  where
  constructor = text (dictModuleName d) <> text "Attrs"
  typesig = text "init" <> constructor <+> colon <> colon
            <+> constructor <+> text "Init"
  decl = text "init" <> constructor <+> equals <+> constructor
  body = encloseStack lbrace rbrace comma $
    [ text n <+> equals <+> toIval (e ^. initial) (e ^. typ)
    | e <- allEntries d
    , let n = userEnumValueName (e ^. name)
    ]
    ++
    [ text n <+> equals <+> lbracket <> rbracket
    | n <- ["rpdos", "tpdos"]
    , not $ null $ pdos d
    ]

toIval :: InitVal -> Type -> Doc
toIval (NumInit x) t@(PrimType (AtomType _)) = text "ival"
  <+> parens (fmtHex x <+> colon <> colon <+> (text $ typeIvoryType t))
toIval (NumInit _) t = toIval NoInit t
toIval (NumInitOffset x) t@(PrimType (AtomType _)) = toIval (NumInit x) t
toIval (NumInitOffset _) t = toIval NoInit t
toIval _ t@(RecordType _ es) = text "istruct"
  <+> encloseStack lbracket rbracket comma
        [ text (typeModuleName t) <> dot <> text (e ^. name) <+> text ".=" <+> toIval (e ^. initial) (e ^. typ)
        | e <- es ]
toIval NoInit _ = text "izero"

objdictTower :: Dict -> Doc
objdictTower d =
  stack [typedef, decl, indent 2 body]
  where
  constructor = text (dictModuleName d) <> text "Attrs"
  typedef = text "objDictTower ::" <+> constructor <+> text "Attr -> Tower e ObjDict"
  decl = text "objDictTower attrs@" <> constructor <> text "{..} = do"
  init' = text "init"
  getter = text "mux_get"
  getter_result = text "result_get"
  setter = text "muxpack_set"
  setter_result = text "result_set"
  dictIface = [ init', getter, getter_result, setter, setter_result ]
  ifaceChans = [ parens (x <> text "_in," <+> x <> text "_out") <+> chan
               | x <- dictIface ]

  chan = text "<- channel"

  attrChans = stack
    [ stack [ onlyReadable rw (text (e ^. name) <> text "_get" <+> chan)
            , onlyWritable rw (text (e ^. name) <> text "_set" <+> chan) ]
    | e <- allEntries d
    , let rw = (e ^. access)
    ]

  attrServer Read = text "readableAttrServer"
  attrServer Const = text "readableAttrServer"
  attrServer Write = text "writableAttrServer"
  attrServer ReadWrite = text "readwritableAttrServer"
  attrServer Reserved = empty
  attrServers =  stack
    [     lparen
      <>  onlyReadable rw (text n <> text "_get_res" <> comma)
      <+>  onlyWritable rw (text n <> text "_set_res" <> comma)
      <+>  text n <> text "_init"
      <>  rparen
      <+> text "<-"
      <+> attrServer rw
      <+> text (userEnumValueName n)
      <+> onlyReadable rw (parens (text "snd" <+> text n <> text "_get"))
      <+> onlyWritable rw (parens (text "snd" <+> text n <> text "_set"))
    | e <- allEntries d
    , let n = (e ^. name)
    , let rw = (e ^. access)
    ]

  mirror n = text n <> text "_local <- stateInit"
             <+> dquotes (text n <> text "_local")
             <+> text n <> text "_init"

  entriesComplexType = filter (isComplexEntry) $ allEntries d
  mirrors = map (mirror . (^. name)) entriesComplexType

  emitterName = (text "emitter_"<>)
  emitter n en = (emitterName n <+> text "<- emitter" <+> en <+> int 1)

  callbackVar = (text "cbref_"<>)
  callback varname = (text "callback" <+> text "$ \\" <> callbackVar varname <+> text "-> do")

  handler n = text "handler"
    <+> n
    <+> dquotes (n <> text "_handler") <+> text "$ do"

  condAddr addr = text "address ==?" <+> fmtHex addr <+> text "==> do"
  condSub subindex = text "subindex ==?" <+> integer subindex <+> text "==> do"
  condArray len = text "subindex >=? 1 .&& subindex <=?" <+> integer len <+> text "==> do"

  packCbVar n = stack [
      text "let sz = packsize" <+> parens (callbackVar n)
    , text "packInto (getres ~> getres_buf ~> stringDataL) 0" <+> parens (callbackVar n)
    , text "store (getres ~> getres_buf ~> stringLengthL) sz"
    , text "emit" <+> emitterName (getter_result) <+> parens (text "constRef getres")
    ]

  packArrayVar n = stack [
      text "let ix = toIx $ (safeCast :: Uint8 -> Sint32) (subindex - 1)"
    , text "    sz = packsize" <+> parens (callbackVar n <+> text "! ix")
    , text "packInto (getres ~> getres_buf ~> stringDataL) 0" <+> parens (callbackVar n <+> text "! ix")
    , text "store (getres ~> getres_buf ~> stringLengthL) sz"
    , text "emit" <+> emitterName (getter_result) <+> parens (text "constRef getres")
    ]

  packSize s = stack [
      text "sz <- local $ ival" <+> (parens (int s <+> text ":: Uint8"))
    , text "packInto (getres ~> getres_buf ~> stringDataL) 0 (constRef sz)"
    , text "store (getres ~> getres_buf ~> stringLengthL) 1"
    , text "emit" <+> emitterName (getter_result) <+> parens (text "constRef getres")
    ]

  getHandlerPack n x@(RecordType _ es) = stack [
      text "refCopy" <+> text n <> text "_local" <+> callbackVar (text n)
    , text "subindex <- getmux ~>* sub"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $ packSize (length es)) ]
      ++
      [ condSub i' </>
        (indent 4 $ packCbVar $ text n <+> text "~>" <+> text (typeModuleName x) <> dot <> text (e ^. name))
      | (i', e) <- zip [1..] es ]
      ++
      [ text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound") ]
    ]
  getHandlerPack n (ArrayType _ len _) = stack [
      text "refCopy" <+> text n <> text "_local" <+> callbackVar (text n)
    , text "subindex <- getmux ~>* sub"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $ packSize (fromInteger len))
      , condArray len
          </> (indent 4 $ packArrayVar $ text n)
      , text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound")
      ]
    ]
  getHandlerPack n (VarArrayType x@(RecordType _ [eLen, eArray])) = stack [
      text "refCopy" <+> text n <> text "_local" <+> callbackVar (text n)
    , text "subindex <- getmux ~>* sub"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $
          packCbVar $ text n <+> text "~>" <+> text (typeModuleName x) <> dot <> text (eLen ^. name))
      , condArray (alen $ eArray ^. typ) </> (indent 4 $
          packArrayVar $ text n <+> text "~>" <+> text (typeModuleName x) <> dot <> text (eArray ^. name))
      , text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound")
      ]
    ]
    where
    alen (ArrayType _ l _) = l
    alen _ = undefined

  getHandlerPack _ (VarArrayType _) = error "VarArray only wraps record"
  getHandlerPack n _ = packCbVar (text n)

  emitGetErrFun =
    text "let emitError x = store (getres ~> getres_ok) false"
    <+> text ">> store (getres ~> getres_error) x"
    <+> text ">> emit" <+> emitterName (getter_result) <+> parens (text "constRef getres")

  -- handles get responses from attrs
  getHandler e = handler (text (e ^. name) <> text "_get_res")
    </> (indent 2 $ stack [
        emitter (getter_result) (getter_result <> text "_in")
      , callback (text (e ^. name))
      , indent 2 $ stack [
          text "getres <- local $ izero"
        , text "store (getres ~> getres_ok) true"
        , text "refCopy (getres ~> getres_mux) getmux"
        , onlyComplex e emitGetErrFun
        , getHandlerPack (e ^. name) (e ^. typ)
        ]
      ])

  getHandlers = [ getHandler e | e <- allEntries d, entryReadable e ]
  attrHandler n = text "attrHandler"
    <+> text (userEnumValueName n)
    <+> text "$ callback $ refCopy"
    <+> text n <> text "_local"
  attrMirrorHandlers =
    map (attrHandler . (^. name)) (filter (entryReadable) entriesComplexType)

  getEmitter n = emitter (text n) $ parens (text "fst" <+> text n <> text "_get")
  getEmitters = [ getEmitter (e ^. name) | e <- allEntries d, entryReadable e ]

  -- get request cond(s)
  getConds = [ condAddr (e ^. index)
               </> indent 4 (text "emitV" <+> emitterName (text (e ^. name)) <+> text "true")
             | e <- allEntries d, entryReadable e ]
              ++
             [ condAddr (e ^. index) </> (indent 4 $ text "emitError writeOnly")
             | e <- allEntries d, not $ entryReadable e ]
             ++
             [ text "true ==> do" </> (indent 4 $ text "emitError notFound") ]

  -- handles get requests from upstream
  getRequestHandler = handler (getter <> text "_out")
    </> (indent 2 $ stack [
        stack getEmitters
      , emitter (getter_result) (getter_result <> text "_in")
      , callback (text "mux")
      , indent 2 $ stack [
          text "refCopy getmux" <+> callbackVar (text "mux")
        , text "address <- getmux ~>* addr"
        , text "subindex <- getmux ~>* sub"
        , empty
        , text "getres <- local $ izero"
        , text "store (getres ~> getres_ok) true"
        , emitGetErrFun
        , empty
        , text "cond_"
        , indent 2 $ encloseStack lbracket rbracket comma $ getConds
        ]
      ])

  attrEmitter n = emitterName (text n) <+> text "<- attrEmitter" <+> text (userEnumValueName n)
  attrEmitters = [ attrEmitter (e ^. name) | e <- allEntries d, entryWritable e ]
  allAttrEmitters = [ attrEmitter (e ^. name) | e <- allEntries d ]

  unpackVar n = stack [
      text "val <- local $ izero"
    , text "let sz = packsize (constRef val)"
    , text "in_sz <-" <+> callbackVar (text "muxpack") <+> text "~> mp_buf ~>* stringLengthL"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $ [
        text "sz ==? in_sz ==> do"
        </> (indent 4 $ stack [
              text "unpackFrom (" <> callbackVar (text "muxpack") <+> text "~> mp_buf ~> stringDataL) 0 val"
            , text "emit" <+> emitterName (text n) <+> parens (text "constRef val")
            , text "emit" <+> emitterName (setter_result) <+> parens (text "constRef setres")
            ])
        -- XXX: handle size high/low?
        , text "sz /=? in_sz ==> do"
        </> (indent 4 $ text "emitError sizeMismatch")
      ]
    ]

  unpackRecordVar n field = stack [
      text "val <- local $ izero"
    , text "let sz = packsize (constRef val)"
    , text "in_sz <-" <+> callbackVar (text "muxpack") <+> text "~> mp_buf ~>* stringLengthL"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $ [
        text "sz ==? in_sz ==> do"
        </> (indent 4 $ stack [
              text "unpackFrom (" <> callbackVar (text "muxpack") <+> text "~> mp_buf ~> stringDataL) 0 val"
            , text "refCopy" <+> parens (text n <> text "_local" <+> text "~>" <+> field) <+> text "val"
            , text "emit" <+> emitterName (text n) <+> parens (text "constRef" <+> text n <> text "_local")
            , text "emit" <+> emitterName (setter_result) <+> parens (text "constRef setres")
            ])
        -- XXX: handle size high/low?
        , text "sz /=? in_sz ==> do"
        </> (indent 4 $ text "emitError sizeMismatch")
      ]
    ]

  unpackArrayVar n field = stack [
      text "val <- local $ izero"
    , text "let ix = toIx $ (safeCast :: Uint8 -> Sint32) subindex"
    , text "let sz = packsize (constRef (" <+> text n <> text "_local" <> field <+> text "! ix))"
    , text "in_sz <-" <+> callbackVar (text "muxpack") <+> text "~> mp_buf ~>* stringLengthL"
    , text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $ [
        text "sz ==? in_sz ==> do"
        </> (indent 4 $ stack [
              text "unpackFrom (" <> callbackVar (text "muxpack") <+> text "~> mp_buf ~> stringDataL) 0 val"
            , text "refCopy" <+> parens (text n <> text "_local" <+> field <> text "! ix") <+> text "val"
            , text "emit" <+> emitterName (text n) <+> parens (text "constRef" <+> text n <> text "_local")
            , text "emit" <+> emitterName (setter_result) <+> parens (text "constRef setres")
            ])
        -- XXX: handle size high/low?
        , text "sz /=? in_sz ==> do"
        </> (indent 4 $ text "emitError sizeMismatch")
      ]
    ]

  setHandlerUnpack n x@(RecordType _ es) = stack
    [ text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $ text "emitError readOnly") ]
      ++
      [ condSub i' </>
        (indent 4 $ unpackRecordVar n (text (typeModuleName x) <> dot <> text (e ^. name)))
      | (i', e) <- zip [1..] es ]
      ++
      [ text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound") ]
    ]
  setHandlerUnpack n (ArrayType _ len _) = stack
    [ text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $ text "emitError readOnly")
      , condArray len </> (indent 4 $ unpackArrayVar n empty)
      , text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound")
      ]
    ]

  setHandlerUnpack n (VarArrayType x@(RecordType _ [eLen, eArray])) = stack
    [ text "cond_"
    , indent 2 $ encloseStack lbracket rbracket comma $
      [ condSub 0 </> (indent 4 $
          unpackRecordVar n (
            text (typeModuleName x) <> dot <> text (eLen ^. name))
          )
      , condArray (alen $ eArray ^. typ) </> (indent 4 $
          unpackArrayVar n (
            text "~>" <+> text (typeModuleName x) <> dot <> text (eArray ^. name)
            )
          )
      , text "true ==> do" </> (indent 4 $ text "emitError subindexNotFound")
      ]
    ]
    where
    alen (ArrayType _ l _) = l
    alen _ = undefined

  setHandlerUnpack n _ = unpackVar n


  -- set request cond(s)
  setConds = [ condAddr (e ^. index) </> (indent 4 $ setHandlerUnpack (e ^. name) (e ^. typ))
             | e <- allEntries d, entryWritable e ]
             ++
             [ condAddr (e ^. index) </> (indent 4 $ text "emitError readOnly")
             | e <- allEntries d, not $ entryWritable e ]
             ++
             [ text "true ==> do" </> (indent 4 $ text "emitError notFound") ]


  setRequestHandler = handler (setter <> text "_out")
    </> (indent 2 $ stack [
        emitter setter_result (setter_result <> text "_in")
      , stack attrEmitters
      , callback (text "muxpack")
      , indent 2 $ stack [
          text "refCopy setmux" <+> callbackVar (text "muxpack")
        , text "address <- setmux ~> mp_mux ~>* addr"
        , text "subindex <- setmux ~> mp_mux ~>* sub"
        , empty
        , text "setres <- local $ izero"
        , text "store (setres ~> setres_ok) true"
        , text "refCopy (setres ~> setres_mux) (setmux ~> mp_mux)"
        , text "let emitError x = store (setres ~> setres_ok) false"
          <+> text ">> store (setres ~> setres_error) x"
          <+> text ">> emit" <+> emitterName (setter_result) <+> parens (text "constRef setres")
        , empty
        , text "cond_"
        , indent 2 $ encloseStack lbracket rbracket comma $ setConds
        ]
      ])

  initHandler = handler (init' <> text "_out")
    </> (indent 2 $ stack [
        stack allAttrEmitters
      , text "callback $ const $ do"
      , indent 2 $ stack [
          text (e ^. name) <> text "_inits" <+> text "<- local" <+> text (e ^. name) <> text "_init"
          </> text "emit" <+> emitterName (text (e ^. name)) <+> parens (text "constRef" <+> text (e ^. name) <> text "_inits")
          | e <- allEntries d
        ]
      ])

  monitor = text "monitor" <+> dquotes (text "objDictMon") <+> text "$ do"
  monitorBody = indent 2 $ stack $ intersperse empty [
      stack mirrors
    , text "getmux <- state" <+> dquotes (text "getmux")
    , text "setmux <- state" <+> dquotes (text "setmux")
    , stack getHandlers
    , stack attrMirrorHandlers
    , getRequestHandler
    , setRequestHandler
    , initHandler
    ]
  --ifc = text $ show $ interfaceMethods i
  body = stack $ intersperse empty [
      stack ifaceChans
    , attrChans
    , attrServers
    , monitor </> monitorBody
    , text "pure $ ObjDict"
      <+> (init' <> text "_in")
      <+> (getter <> text "_in")
      <+> (getter_result <> text "_out")
      <+> (setter <> text "_in")
      <+> (setter_result <> text "_out")
    ]

onlyReadable :: Perm -> Doc -> Doc
onlyReadable = onlyWhen . readable

onlyWritable :: Perm -> Doc -> Doc
onlyWritable = onlyWhen . writable

onlyComplex :: Entry -> Doc -> Doc
onlyComplex = onlyWhen . isComplexEntry

entryReadable :: Entry -> Bool
entryReadable e = readable (e ^. access)

entryWritable :: Entry -> Bool
entryWritable e = writable (e ^. access)

-- is complex entry
isComplexEntry :: Entry -> Bool
isComplexEntry e = isComplex (e ^. typ)

inputFuncName :: String -> String
inputFuncName tn = userEnumValueName tn ++ "Input"

outputFuncName :: String -> String
outputFuncName tn = userEnumValueName tn ++ "Output"
