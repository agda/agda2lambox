{-# LANGUAGE DataKinds, GADTs, FlexibleInstances #-}
-- | Compile targets for the backend
module Agda2Lambox.Compile.Target 
  ( Typing(..)
  , Target(..)
  , WhenTyped(..)
  , getTyped
  , whenTyped
  ) where

import Data.Foldable (Foldable(foldMap))
import Control.DeepSeq ( NFData(rnf) )
import Data.Kind ( Type )

-- | Supported targets.
data Typing = Typed | Untyped

-- | Compile targets, indexed by the typing mode.
data Target :: Typing -> Type where
  ToTyped   :: Target Typed
  ToUntyped :: Target Untyped

-- | Type wrapper that contains a value iff we're in the typed setting.
data WhenTyped (t :: Typing) (a :: Type) :: Type where
  None ::      WhenTyped Untyped a
  Some :: a -> WhenTyped Typed   a

instance Functor (WhenTyped t) where
  fmap f None     = None
  fmap f (Some x) = Some (f x)

instance Applicative (WhenTyped Typed) where
  pure = Some
  Some f <*> Some x = Some (f x)

instance Monad (WhenTyped Typed) where
  Some x >>= f = f x

instance Foldable (WhenTyped t) where
  foldMap f None     = mempty
  foldMap f (Some x) = f x

-- | Retrieve a value when it's there for sure, in the typed setting.
getTyped :: WhenTyped Typed a ->  a
getTyped (Some x) = x

-- | Only perform a computation when targetting typed.
whenTyped :: Applicative m => Target t -> m a -> m (WhenTyped t a)
whenTyped ToUntyped _ = pure None
whenTyped ToTyped   x = Some <$> x

instance NFData (Target t) where
  rnf ToTyped   = ()
  rnf ToUntyped = ()
