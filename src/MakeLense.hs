{-# LANGUAGE GADTs, RankNTypes, TypeOperators, DataKinds, KindSignatures #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module MakeLense (
  Name(..), Proxy(..),
  UnionT,
  Union(..),
  -- keysH, keysU, HasKey,
  Has,
  TypeCheck(..),
  HList(..),
  WrongType, NoSuchKey, NotEnough,
  All,

  (:<)(..),
  sinsert,
  getter,
  setter,
  lenses
  ) where
import GHC.TypeLits
import GHC.Prim (Constraint)
import Data.List (intersperse)
import Data.Proxy
import Lens.Family2
import Lens.Family2.State.Lazy
import Lens.Family2.Unchecked

--

import Haste
import Haste.JSON
import Haste.Foreign
import Haste.Serialize
import Data.Monoid hiding (All)


data (:<) (s :: Symbol) a = Tag a

instance (Show v, KnownSymbol k) => Show (k :< v) where
  show (Tag x :: k :< v) = symbolVal (Name :: Name k) ++ ":" ++ show x

class ShowName k where
  showName :: k -> String

instance (KnownSymbol k) => ShowName (k :< v) where
  showName (_ :: k :< v) = symbolVal (Name :: Name k)

data HList (xs :: [*]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

type family All (ctr :: * -> Constraint) (xs :: [*]) :: Constraint where
  All ctr '[] = ()
  All ctr (x ': xs) = (ctr x, All ctr xs)

class ShowHList (xs :: [*]) where
  showHList :: All Show xs => HList xs -> [String]

instance ShowHList '[] where
  showHList HNil = []

instance ShowHList xs => ShowHList (x ': xs) where
  showHList (HCons x xs) = show x : showHList xs

instance (All Show xs, ShowHList xs) => Show (HList xs) where
  show xs = "[" ++ (concat $ intersperse "," $ showHList xs) ++ "]"

data Union (xs :: [*]) = Union (HList xs) deriving (Show)
type UnionT (xs :: [*]) = Union (Reverse xs)

type family FromUnion y :: [*] where
  FromUnion (Union xs) = xs

class Keys (xs :: [*]) where
  keysH' :: All ShowName xs => HList xs -> [String]

instance Keys '[] where
  keysH' _ = []

instance Keys xs => Keys (x ': xs) where
  keysH' (_ :: HList (x ': xs)) = showName (undefined :: x) : keysH' (undefined :: HList xs)

keysH :: (Keys xs, All ShowName xs) => HList xs -> [String]
keysH xs = reverse $ keysH' xs

keysU :: (Keys xs, All ShowName xs) => Union xs -> [String]
keysU (_ :: Union xs) = keysH (undefined :: HList xs)

class HasKey' (xs :: [*]) (k :: Symbol)
instance HasKey' ((k :< v) ': xs) k
instance {-# OVERLAPPABLE #-} HasKey' xs k => HasKey' ((k' :< v) ': xs) k

type family HasKey y k :: Constraint where
  HasKey (Union xs) k = HasKey' xs k

type Has u (k :: Symbol) out = (HasKey u k, MakeLense (FromUnion u) k out)

type family Snoc (xs :: [*]) y :: [*] where
  Snoc '[] y = '[y]
  Snoc (x ': xs) y = x ': Snoc xs y

type family Reverse (xs :: [*]) :: [*] where
  Reverse '[] = '[]
  Reverse (x ': xs) = Snoc (Reverse xs) x

liftHL :: (HList xs -> HList ys) -> Union xs -> Union ys
liftHL f (Union xs) = Union $ f xs

lowerU :: (Union xs -> Union ys) -> HList xs -> HList ys
lowerU f xs = (\(Union ys) -> ys) $ f (Union xs)

class SmartInsert k v (xs :: [*]) (xs' :: [*]) | k v xs -> xs' where
  sinsert :: (k :< v) -> Union xs -> Union xs'

instance SmartInsert k v '[] '[k :< v] where
  sinsert t = liftHL $ \HNil -> HCons t HNil

instance SmartInsert k v ((k :< v) ': xs) ((k :< v) ': xs) where
  sinsert t = liftHL $ \(HCons _ xs) -> HCons t xs

instance {-# OVERLAPPABLE #-} (SmartInsert k v xs xs') => SmartInsert k v (x ': xs) (x ': xs') where
  sinsert kv (Union (HCons x xs)) = liftHL (HCons x) $ sinsert kv $ Union xs

snoc :: x -> HList xs -> HList (Snoc xs x)
snoc z HNil = HCons z HNil
snoc z (HCons x xs) = HCons x (snoc z xs)

instance {-# OVERLAPPABLE #-} (SmartInsert k v xs xs', Snoc xs' x ~ ys) => SmartInsert k v (x ': xs) ys where
  sinsert kv (Union (HCons x xs)) = liftHL (snoc x) (sinsert kv $ Union xs)

-- pretty error message

class TypeCheck xs xs' where
  check :: Union xs -> Union xs'

instance TypeCheck '[] '[] where
  check = liftHL $ \HNil -> HNil

instance (TypeCheck xs xs') => TypeCheck ((k :< v) ': xs) ((k :< v) ': xs')  where
  check (Union (HCons x xs)) = Union $ HCons x $ lowerU check xs

data NoSuchKey (k :: Symbol)
instance (NoSuchKey k ~ v) => TypeCheck ((k :< v) ': xs) '[]  where
  check = undefined

data WrongType (k :: Symbol) v
data ActualType v
instance {-# OVERLAPPABLE #-} (WrongType k v ~ v') => TypeCheck ((k :< v) ': xs) ((k :< v') ': xs')  where
  check = undefined

instance {-# OVERLAPPABLE #-} (NoSuchKey k ~ v') => TypeCheck ((k :< v) ': xs) ((k' :< v') ': xs')  where
  check = undefined

data NotEnough (k :: Symbol)
instance (NotEnough k ~ v) => TypeCheck '[] ((k :< v) ': xs)  where
  check = undefined

-- makeLense

data Name (s :: Symbol) = Name

class MakeLense (xs :: [*]) (s :: Symbol) out | xs s -> out where
  getter' :: Name (s :: Symbol) -> HList xs -> out
  setter' :: Name (s :: Symbol) -> (out -> out) -> HList xs -> HList xs

instance MakeLense ((k :< v) ': xs) k v where
  getter' _ (HCons (Tag v) _) = v
  setter' _ f (HCons (Tag v) xs) = HCons (Tag $ f v) xs

instance {-# OVERLAPPABLE #-} MakeLense xs syb out => MakeLense ((k :< v) ': xs) syb out where
  getter' k (HCons _ xs) = getter' k xs
  setter' k f (HCons x xs) = HCons x $ setter' k f xs

getter :: (MakeLense xs s out) => Name (s :: Symbol) -> Getter' (Union xs) out
getter syb = to $ \(Union hl) -> getter' syb hl

setter :: (MakeLense xs s out) => Name (s :: Symbol) -> Setter' (Union xs) out
setter syb = setting $ \f (Union hl) -> Union $ setter' syb f hl

lenses :: (MakeLense xs s out) => Name (s :: Symbol) -> Lens' (Union xs) out
lenses syb = lens (^. getter syb) (\u x -> set (setter syb) x u)

-- Haste

instance (Serialize v, KnownSymbol k) => Serialize ((:<) k v) where
  toJSON (Tag t :: k :< v) = Dict [(toJSString $ symbolVal (Name :: Name k), toJSON t)]
  parseJSON (Dict [(_, t)]) = (\t' -> (Tag t' :: k :< v)) <$> parseJSON t

instance (ToAny v) => ToAny ((:<) k v) where
  toAny (Tag t :: k :< v) = toAny t

instance (FromAny v) => FromAny ((:<) k v) where
  fromAny jany = do
    t <- fromAny jany
    return $ (Tag t :: k :< v)

-- {Dict *: JSON} is a monoid
instance Monoid JSON where
  mempty = Dict []
  mappend (Dict xs) (Dict ys) = Dict (xs `mappend` ys)
  mappend _ _ = error "Monoid JSON: out of domain"

class UnionToJSON (xs :: [*]) where
  unionToJSON :: Union xs -> JSON
  jsonToUnion :: JSON -> Parser (Union xs)

instance UnionToJSON '[] where
  unionToJSON (Union HNil) = Dict []
  jsonToUnion (Dict []) = return $ Union HNil

instance (UnionToJSON xs, Serialize x) => UnionToJSON (x ': xs) where
  unionToJSON (Union (HCons x xs)) = toJSON x <> unionToJSON (Union xs)
  jsonToUnion (Dict (x:xs)) = do
    x' <- parseJSON (Dict [x])
    Union xs' <- jsonToUnion (Dict xs)
    return $ Union $ HCons x' xs'

instance (UnionToJSON xs, All Serialize xs) => Serialize (Union xs) where
  toJSON = unionToJSON
  parseJSON = jsonToUnion
