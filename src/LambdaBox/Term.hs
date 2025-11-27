{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
-- | Definition of λ□ terms.
module LambdaBox.Term where

import Data.Bifunctor (first, second)
import Agda.Syntax.Common.Pretty
import Agda2Lambox.Compile.Target
import LambdaBox.Names
import LambdaBox.Type

-- | Definition component in a mutual fixpoint.
data Def (t :: Typing) where
  Def :: { dName :: Name , dBody :: Term t , dArgs :: Int } -> Def t

-- | Mutual components of a fixpoint.
type MFixpoint t = [Def t]

-- | λ□ terms
data Term (t :: Typing) where
  LBox :: Term t -- ^ Proofs and erased terms
  LRel :: Int -> Term t -- ^ Bound variable, with de Bruijn index
  LLambda :: Name -> WhenTyped t Type -> Term t -> Term t -- ^ Lambda abstraction
  LLetIn :: Name -> Term t -> Term t -> Term t
      -- ^ Let bindings.
      --   Unused in the backend, since Agda itself no longer has let bindings
      --   in the concrete syntac.
  LApp :: Term t -> Term t -> Term t -- ^ Term application
  LConst :: KerName -> Term t -- ^ Named constant.
  LConstruct
    :: Inductive
    -> Int
    -> [Term t]
    -> Term t  -- ^ Inductive constructor.
  LCase -- ^ Pattern-matching case construct.
    :: Inductive          -- ^ Inductive type we case on.
    -> Int                -- ^ Number of parameters
    -> Term t             -- ^ Discriminee
    -> [([Name], Term t)] -- ^ Branches
    -> Term t
  LFix -- ^ Fixpoint combinator.
    :: MFixpoint t
    -> Int       -- ^ Index of the fixpoint we keep.
    -> Term t

instance TypeErasure Term where
  erase = \case
    LBox -> LBox
    LRel n -> LRel n
    LLambda n _ t -> LLambda n None (erase t)
    LLetIn n t t' -> LLetIn n (erase t) (erase t')
    LApp t t' -> LApp (erase t) (erase t')
    LConst n -> LConst n
    LConstruct ind i ts -> LConstruct ind i (map erase ts)
    LCase ind i t bs -> LCase ind i (erase t) (map (second erase) bs)
    LFix ds i -> LFix (map erase ds) i

instance TypeErasure Def where
  erase (Def {..}) = Def dName (erase dBody) dArgs

instance Pretty (Def t) where
  -- prettyPrec _ (Def s _ _) = pretty s
  prettyPrec _ (Def _ t _) = pretty t
--
instance Pretty (Term t) where
  prettyPrec p v =
    case v of
      LBox   -> "□"

      LRel k -> "@" <> pretty k

      LLambda n ty t ->
        let getLams :: Term t -> ([(Name , WhenTyped t Type)], Term t)
            getLams (LLambda n ty t) = first ((n,ty):) $ getLams t
            getLams t = ([], t)

            (ns, t') = getLams t

            prettyWhenTyped :: Name -> WhenTyped t Type -> Doc
            prettyWhenTyped n None     = pretty n
            prettyWhenTyped n (Some t) = parens $ pretty n <+> ":" <+> pretty t
        in
        mparens (p > 0) $
        hang ("λ" <+> sep (map (uncurry prettyWhenTyped) ((n,ty):ns)) <+> "→") 2 $ pretty t'


      LLetIn n e t ->
        mparens (p > 0) $ sep
        [ hang ("let" <+> pretty n <+> "=") 2 $ pretty e
        , "in" <+> pretty t
        ]

      LApp u v ->
        mparens (p > 9) $
        hang (pretty u) 2 (prettyPrec 10 v)

      LConst s -> mparens (p > 0) $ pretty s

      LConstruct ind i es ->
        hang (pretty ind <> braces (pretty i)) 2 $
          sep $ map (prettyPrec 10) es

      LCase ind n t bs ->
        mparens (p > 0) $
        sep [ ("case<" <> pretty ind <> "," <> pretty n <> ">") <+> pretty t <+> "of"
            , nest 2 $ vcat (map (\(n, e) -> sep ["λ<" <> pretty n <> ">", nest 2 (pretty e)]) bs) ]

      LFix ds i -> -- FIXME: for mutual recursion
        mparens (p > 0) $
        hang "μ rec ->" 2 $ pretty $ ds !! i
