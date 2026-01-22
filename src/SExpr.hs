{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE DataKinds, MonadComprehensions, GADTs #-}
-- | Converting the LambdaBox AST to s-expressions
module SExpr (ToSexp, prettySexp) where

import Data.Bifunctor(bimap)
import Data.List(intercalate)
import Data.List.NonEmpty qualified as NEL (head)

import Agda.Syntax.Common.Pretty
import LambdaBox
import Agda.Utils.Function (applyWhen)
import Agda2Lambox.Compile.Target
import Data.SExpresso.SExpr
import Data.SExpresso.Print.Lazy
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as LText

type Exp = Sexp Atom

data Sexpable t = forall a. ToSexp t a => S a

-- | SExpr atoms.
data Atom
  = ANode   Text   -- ^ An AST node (i.e a constructor name)
  | AInt    Int    -- ^ A literal integer
  | ABool   Bool   -- ^ A literal boolean
  | AString String -- ^ A literal bytestring


-- | Convert to S-expression (rendered as lazy 'Data.Lazy.Text').
prettySexp :: ToSexp t a => Target t -> a -> LText.Text
prettySexp t = flatPrint printer . toSexp t
  where
  printer :: SExprPrinter () Atom
  printer = mkPrinter \case
    ANode   kw  -> kw
    AInt    i   -> pack $ show i
    ABool   b   -> if b then "true" else "false"
    AString str -> pack $ show str


class ToSexp t a where
  toSexp  :: Target t -> a -> Exp

instance ToSexp t (Sexpable t) where
  toSexp t (S x) = toSexp t x

ctor :: Target t -> Text -> [Sexpable t] -> Exp
ctor t n [] = SAtom (ANode n)
ctor t n xs = SList () (SAtom (ANode n) : map (toSexp t) xs)

instance ToSexp t Atom         where toSexp _   = SAtom
instance ToSexp t (Sexp Atom)  where toSexp _ d = d
instance ToSexp t Int          where toSexp _ s = SAtom (AInt s)
instance ToSexp t Bool         where toSexp _ s = SAtom (ABool s)

instance {-# OVERLAPPING #-} ToSexp t String where
  toSexp _ s = SAtom (AString s)

instance ToSexp t a => ToSexp t (Maybe a) where
  toSexp t x = case x of
    Nothing -> ctor t "None" []
    Just y  -> ctor t "Some" [S y]

instance ToSexp t a => ToSexp t [a] where
  toSexp t xs = SList () $ map (toSexp t) xs

instance (ToSexp t a, ToSexp t b) => ToSexp t (a, b) where
  toSexp t (a, b) = SList () [toSexp t a, toSexp t b]

instance ToSexp t Name where
  toSexp t n = case n of
    Anon    -> ctor t "nAnon"  []
    Named i -> ctor t "nNamed" [S i]

instance ToSexp t ModPath where
  toSexp t = \case
    MPFile dp       -> ctor t "MPfile"  [S dp]
    MPBound dp id i -> ctor t "MPbound" [S dp, S id, S i]
    MPDot mp id     -> ctor t "MPdot"   [S mp, S id]

instance ToSexp t KerName where
  toSexp t KerName{..} = toSexp t (kerModPath, kerName)

instance ToSexp t Inductive where
  toSexp t Inductive{..} = ctor t "inductive" [S indMInd, S indInd]

instance ToSexp t (Def t) where
  toSexp t Def{..} = ctor t "def" [S dName, S dBody, S dArgs]

instance ToSexp t PrimValue where
  toSexp t x = toSexp t (primTag x, primVal x)
    where 
      primTag = \case
        PInt{}    -> ctor t "primInt" []
        PFloat{}  -> ctor t "primFloat" []
        PString{} -> ctor t "primString" []

      primVal = \case
        PInt   i  -> AString (show i)
        PFloat f  -> AString (show f)
        PString s -> AString s


instance ToSexp t (Term t) where
  toSexp t = \case
    LBox                -> ctor t "tBox"       []
    LRel k              -> ctor t "tRel"       [S k]
    LLambda n _ u       -> ctor t "tLambda"    [S n, S u]
    LLetIn n _ u v      -> ctor t "tLetIn"     [S n, S u, S v]
    LApp u v            -> ctor t "tApp"       [S u, S v]
    LConst c            -> ctor t "tConst"     [S c]
    LConstruct ind i es -> ctor t "tConstruct" [S ind, S i, S es]
    LCase ind n u bs    -> ctor t "tCase"      [S (ind, n), S u, S bs]
    LFix mf i           -> ctor t "tFix"       [S mf, S i]
    LPrim p             -> ctor t "tPrim"      [S p]

instance ToSexp t Type where
  toSexp t = \case
    TBox      -> ctor t "TBox"   []
    TAny      -> ctor t "TAny"   []
    TArr a b  -> ctor t "TArr"   [S a, S b]
    TApp a b  -> ctor t "TApp"   [S a, S b]
    TVar k    -> ctor t "TVar"   [S k]
    TInd ind  -> ctor t "TInd"   [S ind]
    TConst kn -> ctor t "TConst" [S kn]

instance ToSexp t RecursivityKind where
  toSexp t = \case
    Finite   -> ctor t "Finite"   []
    CoFinite -> ctor t "CoFinite" []
    BiFinite -> ctor t "BiFinite" []

instance ToSexp t AllowedElims where
  toSexp t = \case
    IntoSProp        -> ctor t "IntoSProp"        []
    IntoPropSProp    -> ctor t "IntoPropSProp"    []
    IntoSetPropSProp -> ctor t "IntoSetPropSProp" []
    IntoAny          -> ctor t "IntoAny"          []

instance ToSexp t (ConstructorBody t) where
  toSexp t@ToUntyped Constructor{..} =
    ctor t "constructor_body" [S cstrName, S cstrArgs]

  toSexp t@ToTyped Constructor{..} =
    toSexp t ((cstrName, getTyped cstrTypes), cstrArgs)

instance ToSexp t (ProjectionBody t) where
  toSexp t@ToUntyped Projection{..} = ctor t "projection_body" [S projName]
  toSexp t@ToTyped   Projection{..} = toSexp t (projName, getTyped projType)

instance ToSexp t TypeVarInfo where
  toSexp t TypeVarInfo{..} =
    ctor t "type_var_info"
      [ S tvarName
      , S tvarIsLogical
      , S tvarIsArity
      , S tvarIsSort
      ]

instance ToSexp t (OneInductiveBody t) where
  toSexp t OneInductive{..} =
    ctor t "one_inductive_body" $
      [ S indName
      , S indPropositional
      , S indKElim
      ]
      ++ -- NOTE(flupe): in the SExp format, order is important
      case t of
        ToUntyped -> []
        ToTyped   -> [S $ getTyped indTypeVars]
      ++
      [ S indCtors
      , S indProjs
      ]

instance ToSexp t (MutualInductiveBody t) where
  toSexp t MutualInductive{..} =
    ctor t "mutual_inductive_body"
      [ S indFinite
      , S indPars
      , S indBodies
      ]

instance ToSexp t (ConstantBody t) where
  toSexp t ConstantBody{..} =
    ctor t "constant_body" $
      case t of
        ToUntyped -> []
        ToTyped   -> [S $ getTyped cstType]
      ++ [S cstBody]

instance ToSexp t (GlobalDecl t) where
  toSexp t = \case
    ConstantDecl  body  -> ctor t "ConstantDecl"  [S body]
    InductiveDecl minds -> ctor t "InductiveDecl" [S minds]
    TypeAliasDecl typ   -> ctor t "TypeAliasDecl" [S typ]

instance ToSexp t (GlobalEnv t) where
  toSexp t@ToUntyped (GlobalEnv env) = toSexp t env
  toSexp t@ToTyped   (GlobalEnv env) = toSexp t $ flip map env \(kn, decl) -> ((kn, True), decl)

instance ToSexp t (LBoxModule t) where
  toSexp t@ToUntyped LBoxModule{..} =
    toSexp t (lboxEnv, ctor t "tConst" [S $ NEL.head $ getUntyped lboxMain])
  toSexp t@ToTyped LBoxModule{..} = toSexp t lboxEnv
