{-# LANGUAGE OverloadedStrings #-}
-- | Definition of λ□ types.
module LambdaBox.Type where

import Agda.Syntax.Common.Pretty
import LambdaBox.Names

data Type
  = TBox
  | TAny
  | TArr Type Type
  | TApp Type Type
  | TVar Int
      -- ^ Reference to a type variable.
      --   Uses De Bruijn *levels* and NOT indices.
  | TInd Inductive
  | TConst KerName

-- | Drop the first @n@ domains of a function type.
dropArrows :: Int -> Type -> Type
dropArrows n (TArr _ t) | n > 0 = dropArrows (n - 1) t
dropArrows _ t = t

instance Pretty Type where
  prettyPrec p = \case
    TBox         -> "□"
    TAny         -> "𝕋"
    TArr s t     -> mparens (p > 0) $ prettyPrec 1 s <+> "→" <+> pretty t
    TApp s t     -> mparens (p > 9) $ pretty s <+> prettyPrec 10 t
    TVar n       -> "@" <> pretty n
    TInd ind     -> mparens (p > 0) $ pretty ind
    TConst kname -> pretty kname
