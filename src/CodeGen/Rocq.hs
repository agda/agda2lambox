{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | Generating Rocq code from our LambdaBox AST
module CodeGen.Rocq (prettyRocq) where

import Data.Bifunctor (first)
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

-- | Pretty-print a Rocq-printable term.
prettyRocq :: ToRocq t a => Target t -> a -> String
prettyRocq t = render . procq t

-- | Util to format Rocq constructor with the given arguments (and precedence).
ctorP :: Int -> String -> [Doc] -> Doc
ctorP _ name []   = text name
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

-- | Strings are printed as Rocq bytestrings.
instance {-# OVERLAPPING #-} ToRocq t String where
  procq _ s = text (show s) <> "%bs"

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
    Anon    -> ctorP 0 "nAnon"  []
    Named i -> ctorP 0 "nNamed" [procq t i]

instance ToRocq t ModPath where
  procqP p t mp = case mp of
    MPFile dp   -> ctorP p "MPfile" [procqP 10 t dp]
    MPDot mp id -> ctorP p "MPdot"  [procqP 10 t mp, procqP 10 t id]

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
    LApp c@(LConstruct ind i []) _ | (k, u) <- ctorChain ind i v, k > 1
                        -> ctorP p "Nat.iter"   [pretty k, ctorP 10 "tApp" [procqP 10 t c], procqP 10 t u]
    LApp u v            -> ctorP p "tApp"       [procqP 10 t u, procqP 10 t v]
    LConst c            -> ctorP p "tConst"     [procqP 10 t c]
    LConstruct ind i es -> ctorP p "tConstruct" [procqP 10 t ind, procqP 10 t i, procqP 10 t es]
    LCase ind n u bs    -> ctorP p "tCase"      [procqP 10 t (ind, n), procqP 10 t u, procqP 10 t bs]
    LFix mf i           -> ctorP p "tFix"       [procqP 10 t mf, procqP 10 t i]
    LPrim v             -> ctorP p "tPrim"      [procqP 10 t v]
    where
    ctorChain :: Inductive -> Int -> Term -> (Int, Term)
    ctorChain ind i = \case
      LApp (LConstruct ind' i' []) u | ind' == ind, i' == i -> first succ $ ctorChain ind i u
      u -> (0, u)

instance ToRocq t Type where
  procqP p t v = case v of
    TBox      -> ctorP p "TBox"   []
    TAny      -> ctorP p "TAny"   []
    TArr a b  -> ctorP p "TArr"   [procqP 10 t a, procqP 10 t b]
    TApp a b  -> ctorP p "TApp"   [procqP 10 t a, procqP 10 t b]
    TVar k    -> ctorP p "TVar"   [pretty k]
    TInd ind  -> ctorP p "TInd"   [procqP 10 t ind]
    TConst kn -> ctorP p "TConst" [procqP 10 t kn]

instance ToRocq t PrimValue where
  procqP p _ (PString s) =
    ctorP p "EPrimitive.prim_string" [text escapedS <> "%pstring"]
    where escapedS = "\"" ++ concatMap (\c -> if c == '"' then "\"\"" else [c]) s ++ "\""

instance ToRocq t RecursivityKind where procq _ = text . show
instance ToRocq t AllowedElims    where procq _ = text . show

instance ToRocq t (ConstructorBody t) where
  procq t@ToUntyped Constructor{..} =
    record [ ("cstr_name",  procq t cstrName)
           , ("cstr_nargs", procq t cstrArgs)
           ]

  procq t@ToTyped Constructor{..} =
    procq t ((cstrName, getTyped cstrTypes), cstrArgs)

instance ToRocq t (ProjectionBody t) where
  procq t@ToUntyped Projection{..} = record [("proj_name",  procq t projName)]
  procq t@ToTyped   Projection{..} = procq t (projName, getTyped projType)

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
  procq t ConstantBody{..} =
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
  procq t@ToUntyped (GlobalEnv env) = procq t env
  procq t@ToTyped   (GlobalEnv env) = procq t $ flip map env \(kn, decl) -> ((kn, True), decl)

instance ToRocq t (LBoxModule t) where
  procq t LBoxModule{..} = vsep
    [ vcat
        [ "From Stdlib           Require Import List."
        , "From Stdlib           Require PrimString."
        , "Import PrimString.PStringNotations."
        , "From MetaRocq.Common  Require Import BasicAst Kernames Universes."
        , "From MetaRocq.Utils   Require Import bytestring."
        , typed "From MetaRocq.Erasure Require Import EAst."
                "From MetaRocq.Erasure Require Import EAst ExAst."
        , typed "From Agda2Lambox     Require Import CheckWF Eval."
                "From Agda2Lambox      Require Import CheckWF Eval."
        , "Import ListNotations."
        ]

    , hang (typed "Definition env : global_declarations :=" "Definition env : global_env :=") 2 $
        procq t lboxEnv <> "."

    , typed "Compute @check_wf_glob eflags env."
            "Compute @check_wf_glob eflags (ExAst.trans_env env)."

    , programs (typed "env" "ExAst.trans_env env") lboxMain
    ]
    where
    typed :: Doc -> Doc -> Doc
    typed u ty = case t of ToUntyped -> u; ToTyped -> ty

-- | Evaluate each main program in the given untyped environment.
programs :: Doc -> [Term] -> Doc
programs env mains = vsep $ flip map (zip [1..] $ reverse mains) \(i :: Int, t) ->
  let progname = "prog" <> pretty i in vsep
  [ hang ("Definition " <> progname <> " : program :=") 2 $
      procq ToUntyped (env, t) <> "."
  , "Compute show_program " <> progname <> "."
  ]
