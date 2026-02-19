{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, DataKinds, MonadComprehensions #-}
-- | Generating Rocq code from our LambdaBox AST
module CodeGen.Rocq (prettyRocq) where

import Data.Bifunctor(bimap)
import Data.List(intercalate)
import Data.List.NonEmpty qualified as NEL

import Agda.Syntax.Common.Pretty
import LambdaBox
import Agda.Utils.Function (applyWhen)
import Agda2Lambox.Compile.Target


class ToRocq t a where
  procq  :: Target t -> a -> Doc
  procqP :: Int -> Target t -> a -> Doc
  procq  = procqP 0
  procqP = const procq
  {-# MINIMAL procq | procqP #-}

-- | Shorthand for untyped λ□ pretty-printer.
uprocq :: ToRocq Untyped a => a -> Doc
uprocq = procq ToUntyped

-- | Shorthand for typed λ□ pretty-printer.
tprocq :: ToRocq Typed a => a -> Doc
tprocq = procq ToTyped

-- | Pretty-print a Rocq-printable term.
prettyRocq :: ToRocq t a => Target t -> a -> String
prettyRocq t = render . procq t

-- | Util to format Rocq constructor with the given arguments.
ctor :: String -> [Doc] -> Doc
ctor = ctorP 0
{-# INLINE ctor #-}

-- | Util to format Rocq constructor with the given arguments (and precedence).
ctorP :: Int -> String -> [Doc] -> Doc
ctorP p name []   = text name
ctorP p name args = applyWhen (p >= 10) parens $ text name <?> fsep args

-- | Util to format Rocq record value with the given fields.
record :: [(String, Doc)] -> Doc
record = enclose
       . fsep
       . punctuate semi
       . map \(k, v) -> hang (text k <+> ":=") 2 v
  where enclose x = "{|" <+> x <+> "|}"

instance ToRocq t Doc  where procq _ d = d
instance ToRocq t Int  where procq _ s = pretty s
instance ToRocq t Bool where procq _ v = if v then "true" else "false"

instance {-# OVERLAPPING #-} ToRocq t String where
  procq _ s = text (show s) <> "%bs"
  -- NOTE(flupe): "%bs" to make sure that we produce Rocq bytestrings

instance ToRocq t a => ToRocq t (Maybe a) where
  procqP p t x = case x of
    Nothing -> ctorP p "None" []
    Just y  -> ctorP p "Some" [procqP 10 t y]

instance ToRocq t a => ToRocq t [a] where
  procq t xs = brackets $ fsep $ punctuate ";" $ map (procq t) xs

instance (ToRocq t a, ToRocq t b) => ToRocq t (a, b) where
  procq t (a, b) = parens $ fsep [procq t a <> comma, procq t b]

instance ToRocq t Name where
  procq t n = case n of
    Anon    -> ctor "nAnon"  []
    Named i -> ctor "nNamed" [procq t i]

instance ToRocq t ModPath where
  procqP p t mp = case mp of
    MPFile dp       -> ctorP p "MPfile"  [procqP 10 t dp]
    MPBound dp id i -> ctorP p "MPbound" [procqP 10 t dp, procqP 10 t id, procqP 10 t i]
    MPDot mp id     -> ctorP p "MPdot"   [procqP 10 t mp, procqP 10 t id]

instance ToRocq t KerName where
  procq t KerName{..} = procq t (kerModPath, kerName)

instance ToRocq t Inductive where
  procq t Inductive{..} =
    record [ ("inductive_mind", procq t indMInd)
           , ("inductive_ind",  procq t indInd)
           ]

instance ToRocq t d => ToRocq t (Def d) where
  procq t Def{..} =
    record [ ("dname", procq t dName)
           , ("dbody", procq t dBody)
           , ("rarg",  procq t dArgs)
           ]

instance ToRocq t Term where
  procqP p t v = case v of
    LBox                -> ctorP p "tBox"       []
    LRel k              -> ctorP p "tRel"       [pretty k]
    LLambda n u         -> ctorP p "tLambda"    [procq t n, procqP 10 t u]
    LLetIn n u v        -> ctorP p "tLetIn"     [procq t n, procqP 10 t u, procqP 10 t v]
    LApp u v            -> ctorP p "tApp"       [procqP 10 t u, procqP 10 t v]
    LConst c            -> ctorP p "tConst"     [procqP 10 t c]
    LConstruct ind i es -> ctorP p "tConstruct" [procqP 10 t ind, procqP 10 t i, procqP 10 t es]
    LCase ind n u bs    -> ctorP p "tCase"      [procqP 10 t (ind, n), procqP 10 t u, procqP 10 t bs]
    LFix mf i           -> ctorP p "tFix"       [procqP 10 t mf, procqP 10 t i]

instance ToRocq t Type where
  procqP p t v = case v of
    TBox      -> ctorP p "TBox"   []
    TAny      -> ctorP p "TAny"   []
    TArr a b  -> ctorP p "TArr"   [procqP 10 t a, procqP 10 t b]
    TApp a b  -> ctorP p "TApp"   [procqP 10 t a, procqP 10 t b]
    TVar k    -> ctorP p "TVar"   [pretty k]
    TInd ind  -> ctorP p "TInd"   [procqP 10 t ind]
    TConst kn -> ctorP p "TConst" [procqP 10 t kn]

instance ToRocq t RecursivityKind where
  procq _ rk = case rk of
    Finite   -> ctor "Finite"   []
    CoFinite -> ctor "CoFinite" []
    BiFinite -> ctor "BiFinite" []

instance ToRocq t AllowedElims where
  procq t ae = case ae of
    IntoSProp        -> ctor "IntoSProp"        []
    IntoPropSProp    -> ctor "IntoPropSProp"    []
    IntoSetPropSProp -> ctor "IntoSetPropSProp" []
    IntoAny          -> ctor "IntoAny"          []

instance ToRocq t (ConstructorBody t) where
  procq ToUntyped Constructor{..} =
    record [ ("cstr_name",  uprocq cstrName)
           , ("cstr_nargs", uprocq cstrArgs)
           ]

  procq ToTyped Constructor{..} =
    tprocq ((cstrName, getTyped cstrTypes), cstrArgs)

instance ToRocq t (ProjectionBody t) where
  procq ToUntyped Projection{..} = record [("proj_name",  uprocq projName)]
  procq ToTyped   Projection{..} = tprocq (projName, getTyped projType)

instance ToRocq t TypeVarInfo where
  procq t TypeVarInfo{..} =
    record
      [ ("tvar_name",       procq t tvarName)
      , ("tvar_is_logical", procq t tvarIsLogical)
      , ("tvar_is_arity",   procq t tvarIsArity)
      , ("tvar_is_sort",    procq t tvarIsSort)
      ]

instance ToRocq t (OneInductiveBody t) where
  procq t OneInductive{..} =
    record $
      [ ("ind_name",          procq t indName)
      , ("ind_propositional", procq t indPropositional)
      , ("ind_kelim",         procq t indKElim)
      , ("ind_ctors",         procq t indCtors)
      , ("ind_projs",         procq t indProjs)
      ] ++
      case t of
        ToUntyped -> []
        ToTyped   -> [("ind_type_vars", procq t $ getTyped indTypeVars)]

instance ToRocq t (MutualInductiveBody t) where
  procq t MutualInductive{..} =
    record [ ("ind_finite", procq t indFinite)
           , ("ind_npars",  procq t indPars)
           , ("ind_bodies", procq t indBodies)
           ]

instance ToRocq t (ConstantBody t) where
  procqP p t ConstantBody{..} =
    record $
      ("cst_body", procq t cstBody) :
      case t of
        ToUntyped -> []
        ToTyped   -> [("cst_type", procq t $ getTyped cstType)]

instance ToRocq t (GlobalDecl t) where
  procqP p t decl = case decl of
    ConstantDecl  body  -> ctorP p "ConstantDecl"  [procqP 10 t body]
    InductiveDecl minds -> ctorP p "InductiveDecl" [procqP 10 t minds]
    TypeAliasDecl typ   -> ctorP p "TypeAliasDecl" [procqP 10 t typ]

instance ToRocq t (GlobalEnv t) where
  procq ToUntyped (GlobalEnv env) = uprocq env
  procq ToTyped   (GlobalEnv env) = tprocq $ flip map env \(kn, decl) -> ((kn, True), decl)

instance ToRocq t (LBoxModule t) where
  procq ToUntyped LBoxModule{..} = vsep
    [ vcat
        [ "From Stdlib           Require Import List."
        , "From MetaRocq.Common  Require Import BasicAst Kernames Universes."
        , "From MetaRocq.Utils   Require Import bytestring."
        , "From MetaRocq.Erasure Require Import EAst."
        , "From Agda2Lambox     Require Import CheckWF Eval."
        , "Import ListNotations."
        ]

    , hang "Definition env : global_declarations :=" 2 $
        uprocq lboxEnv <> "."

    , "Compute @check_wf_glob eflags env."

    , vsep $ flip map (zip [1..] $ reverse $ NEL.toList $ getUntyped lboxMain) \(i :: Int, kn) -> 
        let progname = "prog" <> pretty i in vsep
        [ hang ("Definition " <> progname <> " : program :=") 2 $
            uprocq (text "env" :: Doc, LConst kn)
            <> "."
        , "Compute eval_program " <> progname <> "."
        ]
    ]

  procq ToTyped LBoxModule{..} = vsep
    [ vcat
        [ "From Stdlib           Require Import List."
        , "From MetaRocq.Common  Require Import BasicAst Kernames Universes."
        , "From MetaRocq.Utils   Require Import bytestring."
        , "From MetaRocq.Erasure Require Import EAst ExAst."
        , "From Agda2Lambox      Require Import CheckWF Eval."
        , "Import ListNotations."
        ]

    , hang "Definition env : global_env :=" 2 $ tprocq lboxEnv <> "."
    ]
