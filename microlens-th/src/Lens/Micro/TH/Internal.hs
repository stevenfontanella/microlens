{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif

-- Language.Haskell.TH was not marked as Safe before template-haskell-2.12.0
#if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      :  Lens.Micro.TH.Internal
Copyright   :  (C) 2013-2016 Eric Mertens, Edward Kmett; 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

Functions used by "Lens.Micro.TH". This is an internal module and it may go
away or change at any time; do not depend on it.
-}
module Lens.Micro.TH.Internal
(
  -- * Name utilities
  HasName(..),
  newNames,

  -- * Type variable utilities
  HasTypeVars(..),
  typeVars,
  substTypeVars,

  -- * Miscellaneous utilities
  datatypeTypeKinded,
  inlinePragma,
  conAppsT,
  quantifyType, quantifyType',
  tvbToType,
  unSigT,

  -- * Lens functions
  elemOf,
  lengthOf,
  setOf,
  _ForallT,
)
where

import           Data.Monoid
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Maybe
import           Lens.Micro
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype.TyVarBndr
import qualified Language.Haskell.TH.Datatype as D
import qualified Language.Haskell.TH.Datatype.TyVarBndr as D

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Traversable (traverse)
#endif

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Lens' t Name

instance HasName (TyVarBndr_ flag) where
  name = traverseTVName

instance HasName Name where
  name = id

-- | On @template-haskell-2.11.0.0@ or later, if a 'GadtC' or 'RecGadtC' has
-- multiple 'Name's, the leftmost 'Name' will be chosen.
instance HasName Con where
  name f (NormalC n tys)       = (`NormalC` tys) <$> f n
  name f (RecC n tys)          = (`RecC` tys) <$> f n
  name f (InfixC l n r)        = (\n' -> InfixC l n' r) <$> f n
  name f (ForallC bds ctx con) = ForallC bds ctx <$> name f con
#if MIN_VERSION_template_haskell(2,11,0)
  name f (GadtC ns argTys retTy) =
    (\n -> GadtC [n] argTys retTy) <$> f (head ns)
  name f (RecGadtC ns argTys retTy) =
    (\n -> RecGadtC [n] argTys retTy) <$> f (head ns)
#endif

-- | Generate many new names from a given base name.
newNames :: String {- ^ base name -} -> Int {- ^ count -} -> Q [Name]
newNames base n = sequence [ newName (base++show i) | i <- [1..n] ]

-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

instance HasTypeVars (TyVarBndr_ flag) where
  typeVarsEx s f b
    | Set.member (b^.name) s = pure b
    | otherwise              = name f b

instance HasTypeVars Name where
  typeVarsEx s f n
    | Set.member n s = pure n
    | otherwise      = f n

instance HasTypeVars Type where
  typeVarsEx s f (VarT n)             = VarT <$> typeVarsEx s f n
  typeVarsEx s f (AppT l r)           = AppT <$> typeVarsEx s f l <*> typeVarsEx s f r
  typeVarsEx s f (ForallT bs ctx ty)  = ForallT bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f ty
       where s' = s `Set.union` setOf typeVars bs
  typeVarsEx _ _ t@ConT{}             = pure t
  typeVarsEx _ _ t@TupleT{}           = pure t
  typeVarsEx _ _ t@ListT{}            = pure t
  typeVarsEx _ _ t@ArrowT{}           = pure t
  typeVarsEx _ _ t@UnboxedTupleT{}    = pure t
#if MIN_VERSION_template_haskell(2,8,0)
  typeVarsEx s f (SigT t k)           = SigT <$> typeVarsEx s f t
                                             <*> typeVarsEx s f k
#else
  typeVarsEx s f (SigT t k)           = (`SigT` k) <$> typeVarsEx s f t
#endif
#if MIN_VERSION_template_haskell(2,8,0)
  typeVarsEx _ _ t@PromotedT{}        = pure t
  typeVarsEx _ _ t@PromotedTupleT{}   = pure t
  typeVarsEx _ _ t@PromotedNilT{}     = pure t
  typeVarsEx _ _ t@PromotedConsT{}    = pure t
  typeVarsEx _ _ t@StarT{}            = pure t
  typeVarsEx _ _ t@ConstraintT{}      = pure t
  typeVarsEx _ _ t@LitT{}             = pure t
#endif
#if MIN_VERSION_template_haskell(2,10,0)
  typeVarsEx _ _ t@EqualityT{}        = pure t
#endif
#if MIN_VERSION_template_haskell(2,11,0)
  typeVarsEx s f (InfixT  t1 n t2)    = InfixT  <$> typeVarsEx s f t1
                                                <*> pure n
                                                <*> typeVarsEx s f t2
  typeVarsEx s f (UInfixT t1 n t2)    = UInfixT <$> typeVarsEx s f t1
                                                <*> pure n
                                                <*> typeVarsEx s f t2
  typeVarsEx s f (ParensT t)          = ParensT <$> typeVarsEx s f t
  typeVarsEx _ _ t@WildCardT{}        = pure t
#endif
#if MIN_VERSION_template_haskell(2,12,0)
  typeVarsEx _ _ t@UnboxedSumT{}      = pure t
#endif
#if MIN_VERSION_template_haskell(2,15,0)
  typeVarsEx s f (AppKindT t k)       = AppKindT <$> typeVarsEx s f t
                                                 <*> typeVarsEx s f k
  typeVarsEx s f (ImplicitParamT n t) = ImplicitParamT n <$> typeVarsEx s f t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
  typeVarsEx s f (ForallVisT bs ty)   = ForallVisT bs <$> typeVarsEx s' f ty
       where s' = s `Set.union` setOf typeVars bs
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  typeVarsEx _ _ t@MulArrowT{}        = pure t
#endif

#if !MIN_VERSION_template_haskell(2,10,0)
instance HasTypeVars Pred where
  typeVarsEx s f (ClassP n ts) = ClassP n <$> typeVarsEx s f ts
  typeVarsEx s f (EqualP l r)  = EqualP <$> typeVarsEx s f l <*> typeVarsEx s f r
#endif
#if MIN_VERSION_template_haskell(2,19,0)
  typeVarsEx s f (PromotedInfixT  t1 n t2) = PromotedInfixT  <$> typeVarsEx s f t1
                                                             <*> pure n
                                                             <*> typeVarsEx s f t2
  typeVarsEx s f (PromotedUInfixT t1 n t2) = PromotedUInfixT <$> typeVarsEx s f t1
                                                             <*> pure n
                                                             <*> typeVarsEx s f t2
#endif

instance HasTypeVars Con where
  typeVarsEx s f (NormalC n ts) = NormalC n <$> (traverse . _2) (typeVarsEx s f) ts
  typeVarsEx s f (RecC n ts) = RecC n <$> (traverse . _3) (typeVarsEx s f) ts
  typeVarsEx s f (InfixC l n r) = InfixC <$> g l <*> pure n <*> g r
       where g (i, t) = (,) i <$> typeVarsEx s f t
  typeVarsEx s f (ForallC bs ctx c) = ForallC bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f c
       where s' = s `Set.union` Set.fromList (bs ^.. typeVars)
#if MIN_VERSION_template_haskell(2,11,0)
  typeVarsEx s f (GadtC ns argTys retTy) =
    GadtC ns <$> (traverse . _2) (typeVarsEx s f) argTys
             <*> typeVarsEx s f retTy
  typeVarsEx s f (RecGadtC ns argTys retTy) =
    RecGadtC ns <$> (traverse . _3) (typeVarsEx s f) argTys
                <*> typeVarsEx s f retTy
#endif

instance HasTypeVars t => HasTypeVars [t] where
  typeVarsEx s = traverse . typeVarsEx s

instance HasTypeVars t => HasTypeVars (Maybe t) where
  typeVarsEx s = traverse . typeVarsEx s

-- Traverse /free/ type variables
typeVars :: HasTypeVars t => Traversal' t Name
typeVars = typeVarsEx mempty

-- Substitute using a map of names in for /free/ type variables
substTypeVars :: HasTypeVars t => Map Name Name -> t -> t
substTypeVars m = over typeVars $ \n -> fromMaybe n (Map.lookup n m)

-- | Generate an INLINE pragma.
inlinePragma :: Name -> [DecQ]
#if MIN_VERSION_template_haskell(2,8,0)
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
#else
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase True False)]
#endif

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)

-- Construct a 'Type' using the datatype's type constructor and type
-- parameters. Unlike 'D.datatypeType', kind signatures are preserved to
-- some extent. (See the comments for 'dropSigsIfNonDataFam' below for more
-- details on this.)
datatypeTypeKinded :: D.DatatypeInfo -> Type
datatypeTypeKinded di
  = foldl AppT (ConT (D.datatypeName di))
  $ dropSigsIfNonDataFam
  $ D.datatypeInstTypes di
  where
    {-
    In an effort to prevent users from having to enable KindSignatures every
    time that they use lens' TH functionality, we strip off reified kind
    annotations from when:

    1. The kind of a type does not contain any kind variables. If it *does*
       contain kind variables, we want to preserve them so that we can generate
       type signatures that preserve the dependency order of kind and type
       variables. (The data types in test/T917.hs contain examples where this
       is important.) This will require enabling `PolyKinds`, but since
       `PolyKinds` implies `KindSignatures`, we can at least accomplish two
       things at once.
    2. The data type is not an instance of a data family. We make an exception
       for data family instances, since the presence or absence of a kind
       annotation can be the difference between typechecking or not.
       (See T917DataFam in tests/T917.hs for an example.) Moreover, the
       `TypeFamilies` extension implies `KindSignatures`.
    -}
    dropSigsIfNonDataFam :: [Type] -> [Type]
    dropSigsIfNonDataFam
      | isDataFamily (D.datatypeVariant di) = id
      | otherwise                           = map dropSig

    dropSig :: Type -> Type
    dropSig (SigT t k) | null (D.freeVariables k) = t
    dropSig t                                     = t

-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType = quantifyType' Set.empty

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = filter (\tvb -> D.tvName tvb `Set.notMember` exclude)
     $ D.changeTVFlags D.SpecifiedSpec
     $ D.freeVariablesWellScoped (t:concatMap predTypes c) -- stable order

  predTypes :: Pred -> [Type]
#if MIN_VERSION_template_haskell(2,10,0)
  predTypes p = [p]
#else
  predTypes (ClassP _ ts)  = ts
  predTypes (EqualP t1 t2) = [t1, t2]
#endif

-- | Convert a 'TyVarBndr' into its corresponding 'Type'.
tvbToType :: D.TyVarBndr_ flag -> Type
tvbToType = D.elimTV VarT (SigT . VarT)

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

isDataFamily :: D.DatatypeVariant -> Bool
isDataFamily D.Datatype        = False
isDataFamily D.Newtype         = False
isDataFamily D.DataInstance    = True
isDataFamily D.NewtypeInstance = True

----------------------------------------------------------------------------
-- Lens functions which would've been in Lens.Micro if it wasn't “micro”
----------------------------------------------------------------------------

elemOf :: Eq a => Getting (Endo [a]) s a -> a -> s -> Bool
elemOf l x s = elem x (s ^.. l)

lengthOf :: Getting (Endo [a]) s a -> s -> Int
lengthOf l s = length (s ^.. l)

setOf :: Ord a => Getting (Endo [a]) s a -> s -> Set a
setOf l s = Set.fromList (s ^.. l)

_ForallT :: Traversal' Type ([TyVarBndrSpec], Cxt, Type)
_ForallT f (ForallT a b c) = (\(x, y, z) -> ForallT x y z) <$> f (a, b, c)
_ForallT _ other = pure other
