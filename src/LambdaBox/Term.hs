{-# LANGUAGE OverloadedStrings #-}
-- | Definition of λ□ terms.
module LambdaBox.Term where

import Data.Int (Int64)
import Data.Bifunctor (first)
import Agda.Syntax.Common.Pretty
import LambdaBox.Names


-- | Definition component in a mutual fixpoint
data Def t = Def
  { dName :: Name
  , dBody :: t
  , dArgs :: Int
  }

-- | Mutual components of a fixpoint
type MFixpoint = [Def Term]

-- | 
data PrimValue
  = PInt Int64
      -- NOTE(flupe): ^ Should ensure they are restricted to Int63
  | PFloat Int64
  | PString String

-- | λ□ terms
data Term
  = LBox                             -- ^ Proofs and erased terms
  | LRel Int                         -- ^ Bound variable, with de Bruijn index
  | LLambda Name Term                -- ^ Lambda abstraction
  | LLetIn Name Term Term            -- ^ Let bindings
  | LApp Term Term                   -- ^ Term application
  | LConst KerName                   -- ^ Named constant
  | LConstruct Inductive Int [Term]  -- ^ Inductive constructor
  | LCase                            -- ^ Pattern-matching case construct
      Inductive        -- ^ Inductive type we case on
      Int              -- ^ Number of parameters
      Term             -- ^ Discriminee
      [([Name], Term)] -- ^ Branches
  | LFix                             -- ^ Fixpoint combinator
      MFixpoint
      Int       -- ^ Index of the fixpoint we keep
  | LPrim PrimValue
                -- ^ Primitive literal value


instance Pretty t => Pretty (Def t) where
  -- prettyPrec _ (Def s _ _) = pretty s
  prettyPrec _ (Def _ t _) = pretty t


instance Pretty PrimValue where
  pretty (PInt i)    = text $ show i
  pretty (PFloat f)  = text $ show f
  pretty (PString f) = text $ show f


instance Pretty Term where
  prettyPrec p v =
    case v of
      LBox   -> "□"

      LRel k -> "@" <> pretty k

      LLambda n t ->
        let getLams :: Term -> ([Name], Term)
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
        
