{-# LANGUAGE OverloadedStrings #-}
-- | Definitions for λ□ identifiers, names and references.
module LambdaBox.Names where

import Agda.Syntax.Common.Pretty

-- | Identifiers.
type Ident = String

-- | Path to a directory, innermost component first.
type DirPath = [Ident]

-- | The module part of a kernel name.
data ModPath
  = MPFile DirPath
    -- ^ toplevel libraries i.e. .vo files
  | MPDot ModPath Ident
    -- ^ submodules
  deriving (Eq, Ord)

-- | Absolute name of objects in the Rocq kernel.
data KerName = KerName
  { kerModPath :: ModPath
  , kerName    :: Ident
  }
  deriving (Eq, Ord)

-- | Reference to an inductive datatype.
data Inductive = Inductive
  { indMInd :: KerName
    -- ^ The kername of the mututal inductive group it comes from.
  , indInd  :: Int
    -- ^ Which of those is the inductive we care about.
  }
  deriving Eq

-- | Names used in binders
data Name = Anon | Named Ident

instance Pretty ModPath where
  pretty = \case
    MPFile dp  -> cat $ punctuate "." (map pretty $ reverse dp)
    MPDot mp i -> pretty mp <> "." <> pretty i

instance Pretty KerName where
  pretty KerName{..} = pretty kerModPath <> "." <> text kerName

instance Pretty Inductive where
  pretty Inductive{..} = pretty indMInd <> braces (pretty indInd)

instance Pretty Name where
  pretty = \case
    Anon    -> "_"
    Named i -> text i
