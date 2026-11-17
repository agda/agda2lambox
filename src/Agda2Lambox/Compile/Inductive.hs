{-# LANGUAGE OverloadedStrings #-}
-- | Convert Agda datatypes to λ□ inductive declarations.
module Agda2Lambox.Compile.Inductive (compileInductive) where

import Control.Monad.Reader ( liftIO )
import Control.Monad ( forM, unless )
import Data.Foldable ( toList )
import Data.List.NonEmpty qualified as NEL
import Data.Maybe ( fromMaybe )

import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Telescope ( telViewUpTo, piApplyM, teleArgs, telView, flattenTel )
import Agda.TypeChecking.Substitute ( TelV(..) )
import Agda.Compiler.Backend ( getConstInfo, getErasedConArgs, reportSDoc, AddContext (addContext) )
import Agda.Compiler.Treeless.Erase ( computeErasedConstructorArgs )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg )
import Agda.Syntax.Internal

import Agda.Utils
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Type
import LambdaBox qualified as LBox

-- | Toplevel conversion from a datatype/record definition to a λ□ declaration.
compileInductive :: Target t -> Definition -> CompileM (Maybe (LBox.GlobalDecl t))
compileInductive t defn@Defn{defName} = do
  reportSDoc "agda2lambox.compile.inductive" 5 $
    "Compiling inductive:" <+> prettyTCM defName

  mutuals <- liftTCM $ dataOrRecDefMutuals defn

  reportSDoc "agda2lambox.compile.inductive" 5 $
    "Inductive mutuals:" <+> prettyTCM mutuals

  {- NOTE(flupe):
     if mutuals is []:
       the record/datatype isn't recursive
     if mutuals is [q]:
       then q == defName,
       the record/datatype is inductive/coinductive but not mutually-defined
     otherwise,
       the record/datatype is mutually defined with other things -}

  let items = fromMaybe (NEL.singleton defName) $ NEL.nonEmpty mutuals

  -- ensure that all mutuals get compiled, eventually
  mapM_ requireDef items

  {- also note that we assume the list of mutuals will be the same
     for every record/datatype in the list (especially the order),
     as we make the first item in the list responsible for compiling all of them. -}

  if defName /= NEL.head items then do
    liftIO $ putStrLn $ "Skipping " <> prettyShow defName
    pure Nothing

  else do
    defs <- liftTCM $ mapM getConstInfo items

    unless (all (defIsDataOrRecord . theDef) defs) $
      genericError "Mutually-defined datatypes/records *and* functions not supported."

    pars   <- liftTCM $ toInductivePars defName
    bodies <- forM defs $ compileInductive' t pars

    return $ Just $ LBox.InductiveDecl $ LBox.MutualInductive
      { indFinite = LBox.Finite
      , indPars   = pars
      , indBodies = NEL.toList bodies
      }

-- | Convert one datatype/record of a mutual block with @pars@ parameters.
compileInductive' :: Target t -> Int -> Definition -> CompileM (LBox.OneInductiveBody t)
compileInductive' t pars defn@Defn{defName, defType = indType, theDef} = do
  let npars = getInductiveParams theDef

  params <- theTel <$> telViewUpTo npars indType

  reportSDoc "agda2lambox.compile.inductive" 10 $
    "Inductive parameters:" <+> prettyTCM params

  cpars <- take pars <$> liftTCM (inductivePars defn)
  let pvars :: [Arg Term] = teleArgs params

  -- one type variable per parameter and per index, indices unnamed (as in Rocq)
  TelV arity _ <- telView indType
  tyvars <- whenTyped t $ addContext arity $
    forM (zip [0..] $ flattenTel arity) \(i, dom) -> do
      tv <- liftTCM $ getTypeVarInfo dom
      pure if i < npars then tv else tv { LBox.tvarName = LBox.Anon }

  _ <- whenTyped t $ liftTCM $ computeErasedConstructorArgs defName

  addContext params do
    ctors <- forM (defConstructors theDef) \cname -> do
      reportSDoc "agda2lambox.compile.inductive" 10 $
        "Compiling constructor:" <+> prettyTCM cname
      arity <- liftTCM $ getConArity cname
      reportSDoc "agda2lambox.compile.inductive" 10 $
        "Constructor arity:" <+> prettyTCM arity
      conTypeInfo <- whenTyped t do
        theType <- liftTCM $ defType <$> getConstInfo cname
        conType <- liftTCM $ theType `piApplyM` pvars
        conTel  <- toList . theTel <$> telView conType
        reportSDoc "agda2lambox.compile.inductive" 10 $
          "Constructor telescope:" <+> prettyTCM conTel
        -- constructors take the block's □-params first
        let parArgs = (, LBox.TBox) . domToName <$> cpars
        erased <- liftTCM $ getErasedConArgs cname
        (parArgs ++) <$> compileArgs npars erased conTel
      pure LBox.Constructor
        { cstrName  = qnameToIdent cname
        , cstrArgs  = arity
        , cstrTypes = conTypeInfo
        }

    pure LBox.OneInductive
      { indName          = qnameToIdent defName
      , indPropositional = False
      , indKElim         = LBox.IntoAny
      , indCtors         = ctors
      , indProjs         = []
      , indTypeVars      = tyvars
      }
