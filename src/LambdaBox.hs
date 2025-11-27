{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}
-- | Haskell encoding of the LambdaBox syntax.
module LambdaBox
  ( module LambdaBox.Names
  , module LambdaBox.Term
  , module LambdaBox.Type
  , module LambdaBox.Env
  , emptyName
  , emptyGlobalDecl
  ) where

import Control.Monad.Identity
import Agda2Lambox.Compile.Target
import LambdaBox.Names
import LambdaBox.Term
import LambdaBox.Type
import LambdaBox.Env

-- | Kername for the backed-in empty type.
emptyName :: KerName
emptyName = KerName (MPFile ["LamBox"]) "Empty"

-- | Backed-in definition for the empty type.
--   Used to discard unreachable branches in typed targets.
emptyDecl :: GlobalTermDecl Typed
emptyDecl = InductiveDecl MutualInductive
  { indFinite = Finite
  , indPars   = 0
  , indBodies = [
      OneInductive
        { indName          = "Empty"
        , indPropositional = False
        , indKElim         = IntoAny
        , indTypeVars      = Some []
        , indCtors         = []
        , indProjs         = []
        }
    ]
  }

emptyGlobalDecl :: GlobalDecl Typed
emptyGlobalDecl = GlobalTermDecl (emptyName, emptyDecl)
