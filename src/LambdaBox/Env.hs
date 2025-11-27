{-# LANGUAGE DataKinds, GADTs, OverloadedStrings #-}
{- | 
Module      : LambdaBox.Env
Description : Lambda Box environments, identical for both targets.

In this module, we define all the building blocks that make up
a λ□ global environment.

Because we want to target both the untyped and typed-annotated variants
of λ□, but would rather use a single unified global environment,
we parametrize most definitions with a typing mode 'Agda2Lambox.Compile.Target.Typing'.

This ensures that the typing information /is always there/ when targetting the 
typed target, while also avoiding to do unnecessary work when type information is not required.
-}
module LambdaBox.Env where



import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Bifunctor ( first, second, bimap)
import Data.Either

import Agda.Syntax.Common.Pretty

import LambdaBox.Names
import LambdaBox.Term
import LambdaBox.Type qualified as LBox
import Agda2Lambox.Compile.Target

-- Allowed elimination target for a datatype.
data AllowedElims
  = IntoSProp
  | IntoPropSProp
  | IntoSetPropSProp
  | IntoAny

data RecursivityKind
  = Finite   -- ^ Inductive datatype.
  | CoFinite -- ^ Coinductive datatype.
  | BiFinite -- ^ Non-recursive.

-- | Constructor info in datatype declaration.
data ConstructorBody t = Constructor
  { cstrName  :: Ident
  , cstrArgs  :: Int
  , cstrTypes :: WhenTyped t [(Name, LBox.Type)]
  }

-- | Projection info in datatype declaration.
data ProjectionBody t = Projection
  { projName :: Ident
  , projType :: WhenTyped t LBox.Type
  }

{- | Type variable information.

See [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995)
for the full explanation.

* A type is an /arity/ if it is a (possibly nullary) product into a sort.

    So of the shape @∀ (a₁ : A₁) ... (a_n : A_n) → s@ with @s@ being @Type@ or @Prop@.

    Inhabitants of arities are called /type schemes/.

* A type is /logical/ when it is a proposition (i.e. inhabitants are proofs) 
  or when it is an /arity/ into @Prop@.

    * @P@ when @P : Prop@.
    * @∀ (a₁ : A₁) ... (a_n : A_n) → Prop@ (i.e. inhabitants are propositional type schemes). 

* A type is a sort when it is either @Prop@ or @Type@.

    Note that a sort is always a /nullary/ arity.

A few examples:

* @Type@ is an arity and a sort, but not logical.
* @P@ with @P : Prop@ is logical, but neither an arity nor a sort.
* @Type → Prop@ is logical, an arity, but not a sort.
* @Type → Type@ is an arity, but neither a sort nor logical.
* @∀ (A : Type) → A → A@ is neither of the three.

-}
data TypeVarInfo = TypeVarInfo
  { tvarName      :: Name
  , tvarIsArity   :: Bool
  , tvarIsLogical :: Bool
  , tvarIsSort    :: Bool
  }

-- | Inductive datatype declaration body
data OneInductiveBody t = OneInductive
  { indName          :: Ident
  , indPropositional :: Bool
  , indKElim         :: AllowedElims
  , indTypeVars      :: WhenTyped t [TypeVarInfo]
  , indCtors         :: [ConstructorBody t]
  , indProjs         :: [ProjectionBody t]
  }

-- | Declaration of mutually defined inductive types
data MutualInductiveBody t = MutualInductive
  { indFinite :: RecursivityKind
  , indPars   :: Int
  , indBodies :: [OneInductiveBody t]
  }

-- | Definition of a constant in the environment
data ConstantBody t = ConstantBody
  { cstType :: WhenTyped t ([Name], LBox.Type)
  , cstBody :: Maybe (Term t)
  }

-- | Global declarations.
data GlobalTermDecl (t :: Typing) :: Type where
  ConstantDecl  :: ConstantBody t        -> GlobalTermDecl t
  InductiveDecl :: MutualInductiveBody t -> GlobalTermDecl t

data GlobalTypeDecl (t :: Typing) :: Type where
  TypeAliasDecl :: Maybe ([TypeVarInfo], LBox.Type) -> GlobalTypeDecl Typed

data GlobalDecl (t :: Typing) :: Type where
  GlobalTypeDecl :: WhenTyped t (KerName, GlobalTypeDecl t) -> GlobalDecl t
  GlobalTermDecl :: (KerName, GlobalTermDecl t) -> GlobalDecl t

-- | Global environment.
newtype GlobalEnv t = GlobalEnv [GlobalDecl t]

-- | Generated module
data LBoxModule t = LBoxModule
  { lboxEnv  :: GlobalEnv t
  , lboxMain :: WhenUntyped t (NonEmpty KerName)
  }

mkCoqMod :: Target t -> GlobalEnv Typed -> [KerName] -> CoqModule t
mkCoqMod ToTyped env knames   = CoqModule env knames
mkCoqMod ToUntyped env knames = erase (CoqModule env knames)

-- Type Erasure
----------------------------

instance TypeErasure ConstructorBody where
  erase Constructor {..} = Constructor cstrName cstrArgs None

instance TypeErasure ProjectionBody where
  erase Projection {..} = Projection projName None

instance TypeErasure OneInductiveBody where
  erase OneInductive {..} = OneInductive indName indPropositional indKElim None (map erase indCtors) (map erase indProjs)

instance TypeErasure MutualInductiveBody where
  erase MutualInductive {..} = MutualInductive indFinite indPars (map erase indBodies)

instance TypeErasure ConstantBody where
  erase ConstantBody {..} = ConstantBody None (erase <$> cstBody)

instance TypeErasure GlobalTermDecl where
  erase = \case
    ConstantDecl  cbody -> ConstantDecl (erase cbody)
    InductiveDecl mibody -> InductiveDecl (erase mibody)

instance TypeErasure GlobalDecl where
  erase = \case
    GlobalTermDecl (kn , d) -> GlobalTermDecl (kn , erase d)
    GlobalTypeDecl _  -> GlobalTypeDecl None

instance TypeErasure GlobalEnv where
  erase (GlobalEnv env) =
    GlobalEnv (map erase env)

instance TypeErasure CoqModule where
  erase CoqModule {..} = CoqModule (erase coqEnv) coqPrograms

-- pretty-printing
----------------------------

instance Pretty (ConstructorBody t) where
  pretty Constructor{..} =
    vcat
    [ pretty cstrName <+> parens (pretty cstrArgs <+> "arg(s)")
    , nest 2 $ flip foldMap cstrTypes \args ->
        vcat $ flip map args \(name, typ) ->
          pretty name  <+> ":" <+> pretty typ
    ]

instance Pretty TypeVarInfo where
  pretty TypeVarInfo{..} = pretty tvarName

instance Pretty (OneInductiveBody t) where
  pretty OneInductive{..} = vcat
    [ pretty indName
    , flip foldMap indTypeVars \tvs -> "type variables: " <+> pretty tvs
    , nest 2 $ hang "constructors:" 2 $ vcat $ map pretty indCtors
    ]

instance Pretty (GlobalTermDecl t) where
  pretty = \case
    ConstantDecl ConstantBody{..} ->
      hang "constant declaration:" 2 $ vcat
        [ flip foldMap cstType \(tvs, typ) ->
            vcat [ "type variables:" <+> pretty tvs
                 ,  "type:" <+> pretty typ
                  ]
        , "body:" <+> pretty cstBody
        ]

    InductiveDecl MutualInductive{..} ->
      hang "mutual inductive(s):" 2 $
        vsep $ map pretty indBodies

instance Pretty (GlobalTypeDecl t) where
  pretty = \case
    TypeAliasDecl _ -> "type alias:"

instance Pretty (GlobalDecl t) where
  pretty = \case
    GlobalTermDecl (kn , d) ->
      hang (pretty kn <> ":") 2 (pretty d)
    GlobalTypeDecl (Some (kn , d)) ->
      hang (pretty kn <> ":") 2 (pretty d)
    GlobalTypeDecl None ->
      mempty

instance Pretty (GlobalEnv t) where
  pretty (GlobalEnv env) =
    vsep $ map pretty (reverse env)

instance Pretty (LBoxModule t) where
  pretty LBoxModule{..} = vcat
    [ pretty lboxEnv
    , flip foldMap lboxMain \kn -> "main program:" <+> pretty kn
    ]
