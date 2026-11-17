{-# LANGUAGE DataKinds, GADTs #-}
-- | η-expansions that need the whole environment.
--
--   * MetaRocq's @expanded@, which Peregrine assumes of its input, requires every
--     constructor to be applied to all its parameters and fields.
--
--   * Peregrine dearges typed λ□ (MetaRocq's @dearg_transform@) only if every constant and
--     constructor is applied to at least as many arguments as its dearging mask is long,
--     and otherwise silently keeps every argument of the whole program
--     (@check_dearging_precond@): the applications to fewer arguments are η-expanded,
--     also up to the last parameter of type □ (e.g. an unused one), which dearging keeps
--     in a recursive function, so that it gets □ rather than a variable of another type.
--
--   * Agda erases an argument whose type it erases, also where the parameter has a
--     function type that only the instantiation makes erasable (e.g. @map′ f g@ at
--     @B → T b@): there, □ is η-expanded to a function returning □, as @□ x@ reduces to @□@.
module Agda2Lambox.Compile.Expand ( expandModule ) where

import Data.List ( dropWhileEnd, mapAccumR )
import Data.Map ( Map )
import Data.Map qualified as Map

import Agda2Lambox.Compile.Target ( WhenTyped(..), Typing(Typed), getTyped )
import LambdaBox

data Head = HConst KerName | HCtor KerName Int Int
  deriving (Eq, Ord)

-- | How many arguments a head has to be applied to, and the types of its parameters.
data Sig = Sig Int [Type]

-- | Constants are expanded after their dependencies, which the environment lists last,
--   since the □s passed for the parameters they abstract over may add to their mask.
expandModule :: LBoxModule t -> LBoxModule t
expandModule (LBoxModule (GlobalEnv env) mains) =
  LBoxModule (GlobalEnv env') (map (expandTerm aliases sigs) mains)
  where
  (sigs, env') = mapAccumR decl (Map.fromList $ concatMap ctors env) env
  aliases      = Map.fromList [ (kn, a) | (kn, TypeAliasDecl (Just a)) <- env ]

  decl ss (kn, ConstantDecl cb) = (foldr (Map.insert $ HConst kn) ss $ sig cb', (kn, ConstantDecl cb'))
    where cb' = cb{cstBody = expandTerm aliases ss <$> cstBody cb}
  decl ss d = (ss, d)

-- | A constructor takes all its parameters and fields.
ctors :: (KerName, GlobalDecl t) -> [(Head, Sig)]
ctors = \case
  (kn, InductiveDecl MutualInductive{indPars, indBodies}) ->
    [ (HCtor kn i c, Sig (indPars + cstrArgs ctor) (foldMap (map snd) $ cstrTypes ctor))
    | (i, b) <- zip [0..] indBodies, (c, ctor) <- zip [0..] (indCtors b) ]
  _ -> []

-- | A typed constant takes as many arguments as its mask is long, and its parameters of type □.
sig :: ConstantBody t -> Maybe Sig
sig cb@ConstantBody{cstType = Some (_, ty)} =
  Just $ Sig (maskLength cb `max` length (dropWhileEnd (/= TBox) ds)) ds
  where ds = domains ty
sig _ = Nothing

-- | The number of arrows in a type, through type aliases.
arrows :: Map KerName ([TypeVarInfo], Type) -> Type -> Int
arrows as = \case
  TArr _ b                                                      -> 1 + arrows as b
  t | (TConst k, ts) <- spine t [], Just (_, b) <- Map.lookup k as -> arrows as (subst ts b)
  _                                                             -> 0
  where
  spine (TApp a b) ts = spine a (b : ts)
  spine t          ts = (t, ts)

  subst ts = \case
    TVar i | i < length ts -> ts !! i
    TArr a b               -> TArr (subst ts a) (subst ts b)
    TApp a b               -> TApp (subst ts a) (subst ts b)
    t                      -> t

domains :: Type -> [Type]
domains = \case
  TArr a b -> a : domains b
  _        -> []

-- | The length of a constant's mask, as computed by MetaRocq's @analyze_constant@ and
--   trimmed of its trailing @false@s: the dead λs of its body (through lets), up to
--   the arity of its type, except the first when all arguments are logical;
--   for an axiom, its logical arguments.
maskLength :: ConstantBody Typed -> Int
maskLength ConstantBody{cstType, cstBody} = length $ dropWhileEnd not case cstBody of
  Nothing -> map logical doms
  Just b  -> (if all logical doms then zipWith (&&) (False : repeat True) else id) $
               dead (length doms) b
  where
  doms = domains $ snd $ getTyped cstType

  logical = \case
    TBox -> True
    TAny -> True
    _    -> False

  dead n = \case
    LLetIn _ _ b      -> dead n b
    LLambda _ b | n > 0 -> not (occurs 0 b) : dead (n - 1) b
    _                 -> []

occurs :: Int -> Term -> Bool
occurs k = \case
  LRel i            -> i == k
  LLambda _ b       -> occurs (k + 1) b
  LLetIn _ e b      -> occurs k e || occurs (k + 1) b
  LApp u v          -> occurs k u || occurs k v
  LConstruct _ _ es -> any (occurs k) es
  LCase _ _ d bs    -> occurs k d || any (\(xs, b) -> occurs (k + length xs) b) bs
  LFix ds _         -> any (occurs (k + length ds) . dBody) ds
  _                 -> False

-- | Shift the variables from @k@ on by @n@.
lift :: Int -> Int -> Term -> Term
lift n k = \case
  LRel i | i >= k   -> LRel (i + n)
  LLambda x b       -> LLambda x (lift n (k + 1) b)
  LLetIn x e b      -> LLetIn x (lift n k e) (lift n (k + 1) b)
  LApp u v          -> LApp (lift n k u) (lift n k v)
  LConstruct i c es -> LConstruct i c (map (lift n k) es)
  LCase i p d bs    -> LCase i p (lift n k d) [ (xs, lift n (k + length xs) b) | (xs, b) <- bs ]
  LFix ds j         -> LFix [ d{dBody = lift n (k + length ds) (dBody d)} | d <- ds ] j
  t                 -> t

expandTerm :: Map KerName ([TypeVarInfo], Type) -> Map Head Sig -> Term -> Term
expandTerm as sigs = go
  where
  go t = case spine t [] of
    (h, es) | Just (Sig n tys) <- (`Map.lookup` sigs) =<< headOf h ->
      let es' = zipWith box (map (arrows as) tys ++ repeat 0) (map go es)
          k   = length es
      in if n > k then eta (take (n - k) $ map (== TBox) (drop k tys) ++ repeat False) h es'
                  else foldl' LApp h es'
    (h, es) -> foldl' LApp (inner h) (map go es)

  -- □ and functions returning □ (e.g. @λ x → □@, where Agda erases @λ x y → □@ to it)
  -- take as many arguments as the parameter's type has arrows
  box n e
    | Just k <- boxes e, k < n = iterate (LLambda Anon) LBox !! n
    | otherwise                = e

  boxes = \case
    LBox        -> Just 0
    LLambda _ b -> (+ 1) <$> boxes b
    _           -> Nothing

  spine (LApp u v) es = spine u (v : es)
  spine h          es = (h, es)

  headOf = \case
    LConst kn                        -> Just $ HConst kn
    LConstruct (Inductive kn i) c [] -> Just $ HCtor kn i c
    _                                -> Nothing

  inner = \case
    LLambda x b       -> LLambda x (go b)
    LLetIn x e b      -> LLetIn x (go e) (go b)
    LConstruct i c es -> LConstruct i c (map go es)
    LCase i p d bs    -> LCase i p (go d) (map (fmap go) bs)
    LFix ds j         -> LFix [ d{dBody = go (dBody d)} | d <- ds ] j
    t                 -> t

-- | Abstract a closed head applied to some arguments over the ones it misses, passing □
--   where given, and let-binding the arguments that are not values,
--   so that they are still evaluated once.
eta :: [Bool] -> Term -> [Term] -> Term
eta bs h = bind []
  where
  n = length bs

  bind vs = \case
    [] -> iterate (LLambda Anon) body !! n
      where body = foldl' LApp h $ map (lift n 0) (reverse vs)
                     ++ zipWith (\b i -> if b then LBox else LRel i) bs [n - 1, n - 2 .. 0]
    e : es | value e   -> bind (e : vs) es
           | otherwise -> LLetIn Anon e $ bind (LRel 0 : map (lift 1 0) vs) (map (lift 1 0) es)

  value = \case
    LRel{}            -> True
    LBox              -> True
    LLambda{}         -> True
    LPrim{}           -> True
    LConstruct _ _ [] -> True
    _                 -> False
