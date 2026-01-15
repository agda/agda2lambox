{-# LANGUAGE OverloadedStrings, GADTs #-}
-- | Definition of λ□ terms.
module LambdaBox.Term where

import Data.Int (Int64)
import Data.Bifunctor (first)
import Agda.Syntax.Common.Pretty
import Data.Kind (Type)

import LambdaBox.Names
import Agda2Lambox.Compile.Target


-- | Definition component in a mutual fixpoint
data Def t = Def
  { dName :: Name
  , dBody :: Term t
  , dArgs :: Int
  }

-- | Mutual components of a fixpoint
type MFixpoint t = [Def t]

-- | 
data PrimValue
  = PInt Int64
      -- NOTE(flupe): ^ Should ensure they are restricted to Int63
  | PFloat Int64
  | PString String

-- | λ□ terms
data Term :: Typing -> Type where
  LBox :: Term t -- ^ Proofs and erased terms
  LRel :: Int -> Term t -- ^ Bound variable, with de Bruijn index
  LLambda :: Name -> Term t -> Term t -- ^ Lambda abstraction
  LLetIn :: Name -> Term t -> Term t -> Term t -- ^ Let bindings
  LApp :: Term t -> Term t -> Term t -- ^ Term application
  LConst :: KerName -> Term t -- ^ Named constant
  LConstruct :: Inductive -> Int -> [Term t] -> Term t -- ^ Inductive constructor
  LCase -- ^ Pattern-matching case construct
    :: Inductive          -- ^ Inductive type we case on
    -> Int                -- ^ Number of parameters
    -> Term t             -- ^ Discriminee
    -> [([Name], Term t)] -- ^ Branches
    -> Term t
  LFix  -- ^ Fixpoint combinator
    :: MFixpoint t
    -> Int -- ^ Index of the fixpoint we keep
    -> Term t
  LPrim :: PrimValue -> Term t -- ^ Primitive literal value


instance Pretty (Def t) where
  -- prettyPrec _ (Def s _ _) = pretty s
  prettyPrec _ (Def _ t _) = pretty t


instance Pretty PrimValue where
  pretty (PInt i)    = text $ show i
  pretty (PFloat f)  = text $ show f
  pretty (PString f) = text $ show f


instance Pretty (Term t) where
  prettyPrec p v =
    case v of
      LBox   -> "□"

      LRel k -> "@" <> pretty k

      LLambda n t ->
        let getLams :: Term t -> ([Name], Term t)
            getLams (LLambda n t) = first (n:) $ getLams t
            getLams t = ([], t)

            (ns, t') = getLams t
        in 
        mparens (p > 0) $
        hang ("λ" <+> sep (map pretty (n:ns)) <+> "→") 2 $ pretty t'

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

      LPrim p -> pretty p
        
