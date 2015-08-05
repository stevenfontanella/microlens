{-# LANGUAGE
CPP,
TemplateHaskell,
RankNTypes,
FlexibleContexts
  #-}

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

module Lens.Micro.TH
(
  -- $compatnote
  Getter,
  Fold,
  -- * Making lenses
  makeLenses,
  makeLensesFor,
  makeLensesWith,
  makeFields,
  -- * Default lens rules
  LensRules,
  DefName(..),
  lensRules,
  lensRulesFor,
  defaultFieldRules,
  camelCaseFields,
  -- * Configuring lens rules
  lensField,
  simpleLenses,
  generateSignatures,
  generateUpdateableOptics,
  generateLazyPatterns,
)
where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Data
import           Data.Either
import           Data.Foldable (toList)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.List (nub, findIndices, stripPrefix, isPrefixOf)
import           Data.Maybe
import           Lens.Micro
import           Language.Haskell.TH

#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable (traverse, sequenceA)
#endif


{- $compatnote

When updates aren't allowed, or when a field simply can't be updated (for instance, in the presence of @forall@), instead of 'Lens' and 'Traversal' we generate 'Getter' and 'Fold'. These aren't true @Getter@ and @Fold@ from lens – they're not sufficiently polymorphic. Beware. (Still, they're compatible, it's just that you can't do some things with them that you can do with original ones.)
-}

type Getter s a = forall r. Getting r s a
type Fold s a = forall r. Applicative (Const r) => Getting r s a

-- Lens functions which would've been in Lens.Micro if it wasn't “micro”

elemOf :: Eq a => Getting (Endo [a]) s a -> a -> s -> Bool
elemOf l x = elem x . toListOf l

lengthOf :: Getting (Endo [a]) s a -> s -> Int
lengthOf l = length . toListOf l

setOf :: Ord a => Getting (Endo [a]) s a -> s -> Set a
setOf l = Set.fromList . toListOf l

_ForallT :: Traversal' Type ([TyVarBndr], Cxt, Type)
_ForallT f (ForallT a b c) = (\(x, y, z) -> ForallT x y z) <$> f (a, b, c)
_ForallT _ other = pure other

coerce :: Const r a -> Const r b
coerce = Const . getConst

-- Utilities

-- Modify element at some index in a list.
setIx :: Int -> a -> [a] -> [a]
setIx i x s
  | i < 0 || i >= length s = s
  | otherwise              = let (l, _:r) = splitAt i s
                             in  l ++ [x] ++ r

-- This is like @rewrite@ from uniplate.
rewrite :: (Data a, Data b) => (a -> Maybe a) -> b -> b
rewrite f mbA = case cast mbA of
  Nothing -> gmapT (rewrite f) mbA
  Just a  -> let a' = gmapT (rewrite f) a
             in  fromJust . cast $ fromMaybe a' (f a')

-- @fromSet@ wasn't always there, and we need compatibility with
-- containers-0.4 to compile on GHC 7.4.
fromSet :: (k -> v) -> Set.Set k -> Map.Map k v
#if MIN_VERSION_containers(0,5,0)
fromSet = Map.fromSet
#else
fromSet f x = Map.fromDistinctAscList [ (k,f k) | k <- Set.toAscList x ]
#endif

overHead :: (a -> a) -> [a] -> [a]
overHead _ []     = []
overHead f (x:xs) = f x : xs

-- Control.Lens.TH

{- |
Generate lenses for a data type or a newtype.

To use, you have to enable Template Haskell:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
@

Then, after declaring the datatype (let's say @Foo@), add @makeLenses ''Foo@ on a separate line:

@
data Foo = Foo {
  _x :: Int,
  _y :: Bool }

makeLenses ''Foo
@

This would generate the following lenses, which can be used to access the fields of @Foo@:

@
x :: 'Lens'' Foo Int
x f foo = (\\x' -> f {_x = x'}) '<$>' f (_x foo)

y :: 'Lens'' Foo Bool
y f foo = (\\y' -> f {_y = y'}) '<$>' f (_y foo)
@

(If you don't want a lens to be generated for some field, don't prefix it with an @_@.)

If you want to creat lenses for many types, you can do it all in one place like this (of course, instead you just can use 'makeLenses' several times if you feel it would be more readable):

@
data Foo = ...
data Bar = ...
data Quux = ...

'concat' '<$>' 'mapM' 'makeLenses' [''Foo, ''Bar, ''Quux]
@

When the data type has type parameters, it's possible for a lens to do a polymorphic update – i.e. change the type of the thing along with changing the type of the field. For instance, with this type:

@
data Foo a = Foo {
  _x :: a,
  _y :: Bool }
@

the following lenses would be generated:

@
x :: 'Lens' (Foo a) (Foo b) a b
y :: 'Lens'' (Foo a) Bool
@

However, when there are several fields using the same type parameter, type-changing updates are no longer possible:

@
data Foo a = Foo {
  _x :: a,
  _y :: a }
@

generates

@
x :: 'Lens'' (Foo a) a
y :: 'Lens'' (Foo a) a
@

Finally, when the type has several constructors, some of fields may not be /always/ present – for those, a 'Traversal' is generated instead. For instance, in this example @y@ can be present or absent:

@
data FooBar
  = Foo { _x :: Int, _y :: Bool }
  | Bar { _x :: Int }
@

and the following accessors would be generated:

@
x :: 'Lens'' FooBar Int
y :: 'Traversal'' FooBar Bool
@

So, to get @_y@, you'd have to either use ('^?') if you're not sure it's there, or ('^?!') if you're absolutely sure (and if you're wrong, you'll get an exception). Setting and updating @_y@ can be done as usual.
-}
makeLenses :: Name -> DecsQ
makeLenses = makeFieldOptics lensRules

{- |
Like 'makeLenses', but lets you choose your own names for lenses:

@
data Foo = Foo {foo :: Int, bar :: Bool}

'makeLensesFor' [(\"foo\", \"fooLens\"), (\"bar\", \"_bar\")] ''Foo
@

would create lenses called @fooLens@ and @_bar@. This is useful, for instance, when you don't want to prefix your fields with underscores and want to prefix /lenses/ with underscores instead.

If you give the same name to different fields, it will generate a 'Traversal' instead:

@
data Foo = Foo {slot1, slot2, slot3 :: Int}

'makeLensesFor' [(\"slot1\", \"slots\"),
               (\"slot2\", \"slots\"),
               (\"slot3\", \"slots\")] ''Foo
@
-}
makeLensesFor :: [(String, String)] -> Name -> DecsQ
makeLensesFor fields = makeFieldOptics (lensRulesFor fields)

{- |
Generate lenses with custom parameters.

This function lets you customise generated lenses; to see what exactly can be changed, look at 'LensRules'. 'makeLenses' is implemented with 'makeLensesWith' – it uses the 'lensRules' configuration (which you can build upon – see the “Configuring lens rules” section).

Here's an example of generating lenses that would use lazy patterns:

@
data Foo = Foo {_x, _y :: Int}

'makeLensesWith' ('lensRules' '&' 'generateLazyPatterns' '.~' True) ''Foo
@

When there are several modifications to the rules, the code looks nicer when you use 'flip':

@
'flip' 'makeLensesWith' ''Foo $
  'lensRules'
    '&' 'generateLazyPatterns' '.~' True
    '&' 'generateSignatures'   '.~' False
@
-}
makeLensesWith :: LensRules -> Name -> DecsQ
makeLensesWith = makeFieldOptics

{- |
Generate overloaded lenses.

This lets you deal with several data types having same fields. For instance, let's say you have @Foo@ and @Bar@, and both have a field named @x@. To avoid those fields clashing, you would have to use prefixes:

@
data Foo a = Foo {
  fooX :: Int,
  fooY :: a }

data Bar = Bar {
  barX :: Char }
@

However, if you use 'makeFields' on both @Foo@ and @Bar@ now, it would generate lenses called @x@ and @y@ – and @x@ would be able to access both @fooX@ and @barX@! This is done by generating a separate class for each field, and making relevant types instances of that class:

@
class HasX s a | s -> a where
  x :: 'Lens'' s a

instance HasX (Foo a) Int where
  x :: 'Lens'' (Foo a) Int
  x = ...

instance HasX Bar Char where
  x :: 'Lens'' Bar Char
  x = ...


class HasY s a | s -> a where
  y :: 'Lens'' s a

instance HasY (Foo a) a where
  y :: 'Lens'' (Foo a) a
  y = ...
@

(There's a minor drawback, tho: you can't perform type-changing updates with these lenses.)

If you only want to make lenses for some fields, you can prefix them with underscores – the rest would be untouched. If no fields are prefixed with underscores, lenses would be created for all fields.

The prefix must be the same as the name of the name of the data type (/not/ the constructor).

If you want to use 'makeFields' on types declared in different modules, you can do it, but then you would have to export the @Has*@ classes from one of the modules – 'makeFields' creates a class if it's not in scope yet, so the class must be in scope or else there would be duplicate classes and you would get an “Ambiguous occurrence” error.

Finally, 'makeFields' is implemented as @'makeLenses' 'camelCaseFields'@, so you can build on 'camelCaseFields' if you want to customise behavior of 'makeFields'.
-}
makeFields :: Name -> DecsQ
makeFields = makeFieldOptics camelCaseFields

{- |
Generate simple (monomorphic) lenses even when type-changing lenses are possible – i.e. 'Lens'' instead of 'Lens' and 'Traversal'' instead of 'Traversal'. Just in case, here's an example of a situation when type-changing lenses would be normally generated:

@
data Foo a = Foo { _foo :: a }
@

Generated lens:

@
foo :: Lens (Foo a) (Foo b) a b
@
-}
simpleLenses :: Lens' LensRules Bool
simpleLenses f r = fmap (\x -> r { _simpleLenses = x}) (f (_simpleLenses r))

{- |
Supply type signatures for the generated lenses.

Disable this if you want to write the signature by yourself – for instance, if the signature should be more restricted, or if you want to write haddocks for the lens (as haddocks are attached to the signature and not to the definition).
-}
generateSignatures :: Lens' LensRules Bool
generateSignatures f r =
  fmap (\x -> r { _generateSigs = x}) (f (_generateSigs r))

{- |
Generate “updateable” optics. When turned off, 'Fold's will be generated instead of 'Traversal's and 'Getter's will be generated instead of 'Lens'es.

This option is useful for types with invariants (also known as “types with smart constructors”) – if you generate updateable optics, anyone would be able to use them to break your invariants.
-}
generateUpdateableOptics :: Lens' LensRules Bool
generateUpdateableOptics f r =
  fmap (\x -> r { _allowUpdates = x}) (f (_allowUpdates r))

{- |
Generate optics using lazy pattern matches. This can allow fields of an undefined value to be initialized with lenses:

@
data Foo = Foo {_x :: Int, _y :: Bool}
  deriving Show

'makeLensesWith' ('lensRules' '&' 'generateLazyPatterns' '.~' True) ''Foo
@

@
> 'undefined' '&' x '.~' 8 '&' y '.~' True
Foo {_x = 8, _y = True}
@

(Without 'generateLazyPatterns', the result would be just 'undefined'.)

The downside of this flag is that it can lead to space-leaks and code-size\/compile-time increases when generated for large records. By default this flag is turned off, and strict optics are generated.

When you have a lazy optic, you can get a strict optic from it by composing with ('$!'):

@
strictOptic = ('$!') . lazyOptic
@
-}
generateLazyPatterns :: Lens' LensRules Bool
generateLazyPatterns f r =
  fmap (\x -> r { _lazyPatterns = x}) (f (_lazyPatterns r))

{- |
This lets you choose which fields would have lenses generated for them and how would those lenses be called. To do that, you provide a function that would take a field name and output a list (possibly empty) of lenses that should be generated for that field.

Here's the full type of the function you have to provide:

@
'Name' ->     -- The datatype lenses are being generated for.
['Name'] ->   -- A list of all fields of the datatype.
'Name' ->     -- The current field.
['DefName']   -- A list of lens names.
@

Most of the time you won't need the first 2 parameters, but sometimes they are useful – for instance, the list of all fields would be useful if you wanted to implement a slightly more complicated rule like “if some fields are prefixed with underscores, generate lenses for them, but if no fields are prefixed with underscores, generate lenses for /all/ fields”.

As an example, here's a function used by default. It strips @_@ off the field name, lowercases the next character after @_@, and skips the field entirely if it doesn't start with @_@:

@
\\_ _ n ->
  case 'nameBase' n of
    \'_\':x:xs -> ['TopName' ('mkName' ('toLower' x : xs))]
    _        -> []
@

You can also generate classes (i.e. what 'makeFields' does) by using @'MethodName' className lensName@ instead of @'TopName' lensName@.
-}
lensField :: Lens' LensRules (Name -> [Name] -> Name -> [DefName])
lensField f r = fmap (\x -> r { _fieldToDef = x}) (f (_fieldToDef r))

lensRules :: LensRules
lensRules = LensRules
  { _simpleLenses    = False
  , _generateSigs    = True
  , _generateClasses = False
  -- , _allowIsos       = True
  , _allowUpdates    = True
  , _lazyPatterns    = False
  -- , _classyLenses    = const Nothing
  , _fieldToDef      = \_ _ n ->
       case nameBase n of
         '_':x:xs -> [TopName (mkName (toLower x:xs))]
         _        -> []
  }

{- |
A modification of 'lensRules' used by 'makeLensesFor'.
-}
lensRulesFor ::
  [(String, String)] {- ^ [(Field Name, Lens Name)] -} ->
  LensRules
lensRulesFor fields = lensRules & lensField .~ mkNameLookup fields

mkNameLookup :: [(String,String)] -> Name -> [Name] -> Name -> [DefName]
mkNameLookup kvs _ _ field =
  [ TopName (mkName v) | (k,v) <- kvs, k == nameBase field]

camelCaseFields :: LensRules
camelCaseFields = defaultFieldRules

camelCaseNamer :: Name -> [Name] -> Name -> [DefName]
camelCaseNamer tyName fields field = maybeToList $ do

  fieldPart <- stripPrefix expectedPrefix (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  expectedPrefix = optUnderscore ++ overHead toLower (nameBase tyName)

  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing

defaultFieldRules :: LensRules
defaultFieldRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True  -- classes will still be skipped if they already exist
  -- , _allowIsos       = False -- generating Isos would hinder field class reuse
  , _allowUpdates    = True
  , _lazyPatterns    = False
  -- , _classyLenses    = const Nothing
  , _fieldToDef      = camelCaseNamer
  }

-- Language.Haskell.TH.Lens

-- Has a 'Name'
class HasName t where
  -- Extract (or modify) the 'Name' of something
  name :: Lens' t Name

instance HasName TyVarBndr where
  name f (PlainTV n) = PlainTV <$> f n
  name f (KindedTV n k) = (`KindedTV` k) <$> f n

instance HasName Name where
  name = id

instance HasName Con where
  name f (NormalC n tys)       = (`NormalC` tys) <$> f n
  name f (RecC n tys)          = (`RecC` tys) <$> f n
  name f (InfixC l n r)        = (\n' -> InfixC l n' r) <$> f n
  name f (ForallC bds ctx con) = ForallC bds ctx <$> name f con

-- Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

instance HasTypeVars TyVarBndr where
  typeVarsEx s f b
    | Set.member (b^.name) s = pure b
    | otherwise              = name f b

instance HasTypeVars Name where
  typeVarsEx s f n
    | Set.member n s = pure n
    | otherwise      = f n

instance HasTypeVars Type where
  typeVarsEx s f (VarT n)            = VarT <$> typeVarsEx s f n
  typeVarsEx s f (AppT l r)          = AppT <$> typeVarsEx s f l <*> typeVarsEx s f r
  typeVarsEx s f (SigT t k)          = (`SigT` k) <$> typeVarsEx s f t
  typeVarsEx s f (ForallT bs ctx ty) = ForallT bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f ty
       where s' = s `Set.union` Set.fromList (bs ^.. typeVars)
  typeVarsEx _ _ t                   = pure t

#if !MIN_VERSION_template_haskell(2,10,0)
instance HasTypeVars Pred where
  typeVarsEx s f (ClassP n ts) = ClassP n <$> typeVarsEx s f ts
  typeVarsEx s f (EqualP l r)  = EqualP <$> typeVarsEx s f l <*> typeVarsEx s f r
#endif

instance HasTypeVars Con where
  typeVarsEx s f (NormalC n ts)     =
    NormalC n <$> (traverse . _2) (typeVarsEx s f) ts
  typeVarsEx s f (RecC n ts)        =
    RecC n <$> (traverse . _3) (typeVarsEx s f) ts
  typeVarsEx s f (InfixC l n r)     =
    InfixC <$> g l <*> pure n <*> g r
      where g (i, t) = (,) i <$> typeVarsEx s f t
  typeVarsEx s f (ForallC bs ctx c) =
    ForallC bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f c
      where s' = s `Set.union` Set.fromList (bs ^.. typeVars)

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

-- FieldTH.hs

------------------------------------------------------------------------
-- Field generation entry point
------------------------------------------------------------------------


-- Compute the field optics for the type identified by the given type name.
-- Lenses will be computed when possible, Traversals otherwise.
makeFieldOptics :: LensRules -> Name -> DecsQ
makeFieldOptics rules tyName =
  do info <- reify tyName
     case info of
       TyConI dec -> makeFieldOpticsForDec rules dec
       _          -> fail "makeFieldOptics: Expected type constructor name"


makeFieldOpticsForDec :: LensRules -> Dec -> DecsQ
makeFieldOpticsForDec rules dec = case dec of
  DataD    _ tyName vars cons _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) cons
  NewtypeD _ tyName vars con  _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) [con]
  DataInstD _ tyName args cons _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) cons
  NewtypeInstD _ tyName args con _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) [con]
  _ -> fail "makeFieldOptics: Expected data or newtype type-constructor"
  where
  mkS tyName vars = tyName `conAppsT` map VarT (toListOf typeVars vars)


-- Compute the field optics for a deconstructed Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldOpticsForDec' :: LensRules -> Name -> Type -> [Con] -> DecsQ
makeFieldOpticsForDec' rules tyName s cons =
  do fieldCons <- traverse normalizeConstructor cons
     let allFields  = toListOf (folded . _2 . folded . _1 . folded) fieldCons
     let defCons    = over normFieldLabels (expandName allFields) fieldCons
         allDefs    = setOf (normFieldLabels . folded) defCons
     perDef <- sequenceA (fromSet (buildScaffold rules s defCons) allDefs)

     let defs = Map.toList perDef
--     case _classyLenses rules tyName of
--       Just (className, methodName) ->
--         makeClassyDriver rules className methodName s defs
--       Nothing -> do decss  <- traverse (makeFieldOptic rules) defs
--                     return (concat decss)

     -- just don't make anything classy
     decss  <- traverse (makeFieldOptic rules) defs
     return (concat decss)

  where

  -- Traverse the field labels of a normalized constructor
  normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
  normFieldLabels = traverse . _2 . traverse . _1

  -- Map a (possibly missing) field's name to zero-to-many optic definitions
  expandName :: [Name] -> Maybe Name -> [DefName]
  expandName allFields (Just n) = _fieldToDef rules tyName allFields n
  expandName _ _ = []


-- Normalized the Con type into a uniform positional representation,
-- eliminating the variance between records, infix constructors, and normal
-- constructors.
normalizeConstructor ::
  Con ->
  Q (Name, [(Maybe Name, Type)]) -- constructor name, field name, field type

normalizeConstructor (RecC n xs) =
  return (n, [ (Just fieldName, ty) | (fieldName,_,ty) <- xs])

normalizeConstructor (NormalC n xs) =
  return (n, [ (Nothing, ty) | (_,ty) <- xs])

normalizeConstructor (InfixC (_,ty1) n (_,ty2)) =
  return (n, [ (Nothing, ty1), (Nothing, ty2) ])

normalizeConstructor (ForallC _ _ con) =
  do con' <- normalizeConstructor con
     return (set (_2 . mapped . _1) Nothing con')


data OpticType = GetterType | LensType -- or IsoType

-- Compute the positional location of the fields involved in
-- each constructor for a given optic definition as well as the
-- type of clauses to generate and the type to annotate the declaration
-- with.
buildScaffold ::
  LensRules                                                                ->
  Type                              {- outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- normalized constructors          -} ->
  DefName                           {- target definition                -} ->
  Q (OpticType, OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold rules s cons defName =

  do (s',t,a,b) <- buildStab s (concatMap snd consForDef)

     let defType
           | Just (_,cx,a') <- a ^? _ForallT =
               let optic | lensCase  = ''Getter
                         | otherwise = ''Fold
               in OpticSa cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase  = ''Getter
                         | otherwise = ''Fold
               in OpticSa [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules || s' == t && a == b =
               let optic -- isoCase && _allowIsos rules = ''Iso'
                         | lensCase                    = ''Lens'
                         | otherwise                   = ''Traversal'
               in OpticSa [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic -- isoCase && _allowIsos rules = ''Iso
                         | lensCase                    = ''Lens
                         | otherwise                   = ''Traversal
               in OpticStab optic s' t a b

         opticType | has _ForallT a            = GetterType
                   | not (_allowUpdates rules) = GetterType
                   -- isoCase                   = IsoType
                   | otherwise                 = LensType

     return (opticType, defType, scaffolds)
  where
  consForDef :: [(Name, [Either Type Type])]
  consForDef = over (mapped . _2 . mapped) categorize cons

  scaffolds :: [(Name, Int, [Int])]
  scaffolds = [ (n, length ts, rightIndices ts) | (n,ts) <- consForDef ]

  rightIndices :: [Either Type Type] -> [Int]
  rightIndices = findIndices (has _Right)

  -- Right: types for this definition
  -- Left : other types
  categorize :: ([DefName], Type) -> Either Type Type
  categorize (defNames, t)
    | defName `elem` defNames = Right t
    | otherwise               = Left  t

  lensCase :: Bool
  lensCase = all (\x -> lengthOf (_2 . folded . _Right) x == 1) consForDef

  -- isoCase :: Bool
  -- isoCase = case scaffolds of
  --             [(_,1,[0])] -> True
  --             _           -> False


data OpticStab = OpticStab     Name Type Type Type Type
               | OpticSa   Cxt Name Type Type

stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) = quantifyType [] (c `conAppsT` [s,t,a,b])
stabToType (OpticSa cx c s   a  ) = quantifyType cx (c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}        = []
stabToContext (OpticSa cx _ _ _) = cx

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = c
stabToOptic (OpticSa _ c _ _) = c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ a) = a

-- Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab s categorizedFields =
  do (subA,a) <- unifyTypes targetFields
     let s' = applyTypeSubst subA s

     -- compute possible type changes
     sub <- sequenceA (fromSet (newName . nameBase) unfixedTypeVars)
     let (t,b) = over both (substTypeVars sub) (s',a)

     return (s',t,a,b)

  where
  (fixedFields, targetFields) = partitionEithers categorizedFields
  fixedTypeVars               = setOf typeVars fixedFields
  unfixedTypeVars             = setOf typeVars s Set.\\ fixedTypeVars


-- Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makeFieldOptic ::
  LensRules ->
  (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) ->
  DecsQ
makeFieldOptic rules (defName, (opticType, defType, cons)) =
  do cls <- mkCls
     sequenceA (cls ++ sig ++ def)
  where
  mkCls = case defName of
          MethodName c n | _generateClasses rules ->
            do classExists <- isJust <$> lookupTypeName (show c)
               return (if classExists then [] else [makeFieldClass defType c n])
          _ -> return []

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n clauses : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makeFieldInstance defType c (fun n)]

  clauses = makeFieldClauses rules opticType cons

------------------------------------------------------------------------
-- Field class generation
------------------------------------------------------------------------

makeFieldClass :: OpticStab -> Name -> Name -> DecQ
makeFieldClass defType className methodName =
  classD (cxt []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,a])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"

makeFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makeFieldInstance defType className =
  instanceD (cxt [])
    (return (className `conAppsT` [stabToS defType, stabToA defType]))

------------------------------------------------------------------------
-- Optic clause generators
------------------------------------------------------------------------

makeFieldClauses :: LensRules -> OpticType -> [(Name, Int, [Int])] -> [ClauseQ]
makeFieldClauses rules opticType cons =
  case opticType of

    -- IsoType    -> [ makeIsoClause conName | (conName, _, _) <- cons ]

    GetterType -> [ makeGetterClause conName fieldCount fields
                    | (conName, fieldCount, fields) <- cons ]

    LensType   -> [ makeFieldOpticClause conName fieldCount fields irref
                    | (conName, fieldCount, fields) <- cons ]
      where
      irref = _lazyPatterns rules
           && length cons == 1

-- Construct an optic clause that returns an unmodified value
-- given a constructor name and the number of fields on that
-- constructor.
makePureClause :: Name -> Int -> ClauseQ
makePureClause conName fieldCount =
  do xs <- replicateM fieldCount (newName "x")
     -- clause: _ (Con x1..xn) = pure (Con x1..xn)
     clause [wildP, conP conName (map varP xs)]
            (normalB (appE (varE 'pure) (appsE (conE conName : map varE xs))))
            []

-- Construct an optic clause suitable for a Getter or Fold
-- by visited the fields identified by their 0 indexed positions
makeGetterClause :: Name -> Int -> [Int] -> ClauseQ
makeGetterClause conName fieldCount []     = makePureClause conName fieldCount
makeGetterClause conName fieldCount fields =
  do f  <- newName "f"
     xs <- replicateM (length fields) (newName "x")

     let pats (i:is) (y:ys)
           | i `elem` fields = varP y : pats is ys
           | otherwise = wildP : pats is (y:ys)
         pats is     _  = map (const wildP) is

         fxs   = [ appE (varE f) (varE x) | x <- xs ]
         body  = foldl (\a b -> appsE [varE '(<*>), a, b])
                       (appE (varE 'coerce) (head fxs))
                       (tail fxs)

     -- clause f (Con x1..xn) = coerce (f x1) <*> ... <*> f xn
     clause [varP f, conP conName (pats [0..fieldCount - 1] xs)]
            (normalB body)
            []

-- Build a clause that updates the field at the given indexes
-- When irref is 'True' the value with me matched with an irrefutable
-- pattern. This is suitable for Lens and Traversal construction
makeFieldOpticClause :: Name -> Int -> [Int] -> Bool -> ClauseQ
makeFieldOpticClause conName fieldCount [] _ =
  makePureClause conName fieldCount
makeFieldOpticClause conName fieldCount (field:fields) irref =
  do f  <- newName "f"
     xs <- replicateM fieldCount          (newName "x")
     ys <- replicateM (1 + length fields) (newName "y")

     let xs' = foldr (\(i,x) -> setIx i x) xs (zip (field:fields) ys)

         mkFx i = appE (varE f) (varE (xs !! i))

         body0 = appsE [ varE 'fmap
                       , lamE (map varP ys) (appsE (conE conName : map varE xs'))
                       , mkFx field
                       ]

         body = foldl (\a b -> appsE [varE '(<*>), a, mkFx b]) body0 fields

     let wrap = if irref then tildeP else id

     clause [varP f, wrap (conP conName (map varP xs))]
            (normalB body)
            []

------------------------------------------------------------------------
-- Unification logic
------------------------------------------------------------------------

-- The field-oriented optic generation supports incorporating fields
-- with distinct but unifiable types into a single definition.

-- Unify the given list of types, if possible, and return the
-- substitution used to unify the types for unifying the outer
-- type when building a definition's type signature.
unifyTypes :: [Type] -> Q (Map Name Type, Type)
unifyTypes (x:xs) = foldM (uncurry unify1) (Map.empty, x) xs
unifyTypes []     = fail "unifyTypes: Bug: Unexpected empty list"


-- Attempt to unify two given types using a running substitution
unify1 :: Map Name Type -> Type -> Type -> Q (Map Name Type, Type)
unify1 sub (VarT x) y
  | Just r <- Map.lookup x sub = unify1 sub r y
unify1 sub x (VarT y)
  | Just r <- Map.lookup y sub = unify1 sub x r
unify1 sub x y
  | x == y = return (sub, x)
unify1 sub (AppT f1 x1) (AppT f2 x2) =
  do (sub1, f) <- unify1 sub  f1 f2
     (sub2, x) <- unify1 sub1 x1 x2
     return (sub2, AppT (applyTypeSubst sub2 f) x)
unify1 sub x (VarT y)
  | elemOf typeVars y (applyTypeSubst sub x) =
      fail "Failed to unify types: occurs check"
  | otherwise = return (Map.insert y x sub, x)
unify1 sub (VarT x) y = unify1 sub y (VarT x)

-- TODO: Unify contexts
unify1 sub (ForallT v1 [] t1) (ForallT v2 [] t2) =
     -- This approach works out because by the time this code runs
     -- all of the type variables have been renamed. No risk of shadowing.
  do (sub1,t) <- unify1 sub t1 t2
     v <- fmap nub (traverse (limitedSubst sub1) (v1++v2))
     return (sub1, ForallT v [] t)

unify1 _ x y = fail ("Failed to unify types: " ++ show (x,y))

-- Perform a limited substitution on type variables. This is used
-- when unifying rank-2 fields when trying to achieve a Getter or Fold.
limitedSubst :: Map Name Type -> TyVarBndr -> Q TyVarBndr
limitedSubst sub (PlainTV n)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (PlainTV m)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst sub (KindedTV n k)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (KindedTV m k)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst _ tv = return tv

-- Apply a substitution to a type. This is used after unifying
-- the types of the fields in unifyTypes.
applyTypeSubst :: Map Name Type -> Type -> Type
applyTypeSubst sub = rewrite aux
  where
  aux (VarT n) = Map.lookup n sub
  aux _        = Nothing

------------------------------------------------------------------------
-- Field generation parameters
------------------------------------------------------------------------

{- |
Rules used to generate lenses. You can't create them from scratch, but you can customise already existing ones with lenses in the “Configuring lens rules” section.

For an example, see 'makeLensesWith'.
-}
data LensRules = LensRules
  { _simpleLenses    :: Bool
  , _generateSigs    :: Bool
  , _generateClasses :: Bool
  -- , _allowIsos       :: Bool
  , _allowUpdates    :: Bool -- Allow Lens/Traversal (otherwise Getter/Fold)
  , _lazyPatterns    :: Bool
  -- Type Name -> Field Names -> Target Field Name -> Definition Names
  , _fieldToDef      :: Name -> [Name] -> Name -> [DefName]
  -- , _classyLenses    :: Name -> Maybe (Name,Name)
       -- type name to class name and top method
  }

{- |
Name to give to a generated lens.
-}
data DefName
  = TopName Name          -- ^ Simple top-level definiton name
  | MethodName Name Name  -- ^ 'makeFields'-style class name and method name
  deriving (Show, Eq, Ord)

------------------------------------------------------------------------
-- Miscellaneous utility functions
------------------------------------------------------------------------


-- Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t))

-- This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t Set.\\ exclude))


------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]

#ifdef INLINING

#if MIN_VERSION_template_haskell(2,8,0)

# ifdef OLD_INLINE_PRAGMAS
-- 7.6rc1?
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase Inline False)]
# else
-- 7.7.20120830
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
# endif

#else
-- GHC <7.6, TH <2.8.0
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase True False)]
#endif

#else

inlinePragma _ = []

#endif

-- Control.Lens.Internal.TH

-- Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)
