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
  , cstBody :: Maybe Term
  }

-- | Global declarations.
data GlobalDecl (t :: Typing) :: Type where
  ConstantDecl  :: ConstantBody t                   -> GlobalDecl t
  InductiveDecl :: MutualInductiveBody t            -> GlobalDecl t
  TypeAliasDecl :: Maybe ([TypeVarInfo], LBox.Type) -> GlobalDecl Typed

-- | Global environment.
newtype GlobalEnv t = GlobalEnv [(KerName, GlobalDecl t)]

-- | Generated module
data LBoxModule t = LBoxModule
  { lboxEnv  :: GlobalEnv t
  , lboxMain :: WhenUntyped t (NonEmpty KerName)
  }


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

instance Pretty (GlobalDecl t) where
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

    TypeAliasDecl decl ->
      hang "type alias:" 2 $ case decl of
        Nothing -> mempty
        Just (vars, typ) -> vcat
          [ "type variables:" <+> pretty vars
          , "type:" <+> pretty typ
          ]

instance Pretty (GlobalEnv t) where
  pretty (GlobalEnv env) =
    vsep $ flip map (reverse env) \(kn, d) ->
      hang (pretty kn <> ":") 2 (pretty d)

instance Pretty (LBoxModule t) where
  pretty LBoxModule{..} = vcat
    [ pretty lboxEnv
    , flip foldMap lboxMain \kn -> "main program:" <+> pretty kn
    ]
