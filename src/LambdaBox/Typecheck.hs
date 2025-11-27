{-# LANGUAGE DataKinds #-}
module LambdaBox.Typecheck where

import Agda2Lambox.Compile.Target
import LambdaBox
import Agda.Utils.Monad

type Env = Int -> Type

extend :: Env -> Type -> Env
extend env ty 0 = ty
extend env ty i = env (i - 1)

-- | Infer the type of a term
infer :: MonadFail m => Term Typed -> Env -> m Type
infer term env =
  case term of
    LBox -> return TBox
    LRel n -> return $ env n
    LLambda n (Some ty) b ->
      infer b (extend env ty)
    LLetIn n t b -> do
      ty <- infer t env
      ty' <- infer b (extend env ty)
      return (TArr ty ty')
    LApp t t' -> do
      TArr t1 t2 <- infer t env
      ifM (check t' t1 env)
        (return t2)
        (fail "Failure")
    LConst kn -> fail "Not implemented" -- FIXME
    LConstruct ind i ts -> fail "Not implemented" -- FIXME
    LCase ind n t bs -> fail "Not implemented" -- FIXME
    LFix mfix i -> fail "Not implemented" -- FIXME

-- | Check a term against a type
check :: MonadFail m => Term Typed -> Type -> Env -> m Bool
check term ty env = case term of
  LBox -> return $ TBox == ty
  LRel n -> return $ env n == ty
  LLambda n (Some ty) b -> case ty of
    TArr t1 t2 -> check b t2 (extend env t1)
    _ -> return False
  LLetIn n t b -> do
    ty' <- infer t env
    check b ty (extend env ty')
  LApp t t' -> do
    TArr t1 t2 <- infer t env
    check t' t1 env
  LConst c -> return False
  LConstruct ind n ts -> return False
  LCase ind i t bs -> return False
  LFix mfix i -> return False

typecheck :: GlobalEnv Typed -> Bool
typecheck _ = False
