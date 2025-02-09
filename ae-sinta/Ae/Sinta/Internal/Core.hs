{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ae.Sinta.Internal.Core where

import Ae.Sinta.Internal.Types (Eff', Ret, S')

import Prelude hiding (max)

import Data.Pointed (Pointed (point))

import GHC.Base ( Symbol )
import GHC.TypeLits ( KnownSymbol )
import GHC.Records ( HasField(..) )

import Effectful ( Effect, Eff, (:>) )
import Effectful.TH ( makeEffect ) 
import Effectful.Dispatch.Dynamic
    ( interpret, interpret_, localSeqBorrow, localSeqUnlift )

-- * Expressions

-- ** Lit

data Lit f t :: Effect where
    Lit :: t -> Lit f t m (f t)

makeEffect ''Lit

runLit :: forall t f es fullEs a.
    (fullEs ~ Lit f t : es)
    => (t -> Eff' fullEs f t) -> Eff fullEs a -> Eff es a
runLit lift = interpret $ \env -> \case
    Lit value -> localSeqBorrow @'[Lit f t] env \borrow -> borrow (lift value)

-- ** Lit1

data Lit1 f t :: Effect where
    Lit1 :: t a -> Lit1 f t m (f (t a))

makeEffect ''Lit1

runLit1 :: forall t f es fullEs n.
    (fullEs ~ Lit1 f t : es)
    => (forall a. t a -> Eff' fullEs f (t a)) -> Eff fullEs n -> Eff es n
runLit1 lift = interpret $ \env -> \case
    Lit1 value -> localSeqBorrow @'[Lit1 f t] env \borrow -> borrow (lift value)

runLit1' :: forall t f es a.
    (Pointed f) => Eff (Lit1 f t : es) a -> Eff es a
runLit1' = interpret_ \case Lit1 value -> pure (point value)

-- ** Lit2

data Lit2 f t :: Effect where
    Lit2 :: t a b -> Lit2 f t m (f (t a b))

makeEffect ''Lit2

runLit2 :: forall t f es fullEs n.
    (fullEs ~ Lit2 f t : es)
    => (forall a b. t a b -> Eff' fullEs f (t a b)) -> Eff fullEs n -> Eff es n
runLit2 lift = interpret $ \env -> \case
    Lit2 value -> localSeqBorrow @'[Lit2 f t] env \borrow -> borrow (lift value)

runLit2' :: forall t f es a.
    (Pointed f) => Eff (Lit2 f t : es) a -> Eff es a
runLit2' = interpret_ \case Lit2 value -> pure (point value)

-- ** Lit3

data Lit3 f t :: Effect where
    Lit3 :: t a b c -> Lit3 f t m (f (t a b c))

makeEffect ''Lit3

runLit3 :: forall t f es fullEs n.
    (fullEs ~ Lit3 f t : es)
    => (forall a b c. t a b c -> Eff' fullEs f (t a b c)) -> Eff fullEs n -> Eff es n
runLit3 lift = interpret $ \env -> \case
    Lit3 value -> localSeqBorrow @'[Lit3 f t] env \borrow -> borrow (lift value)

runLit3' :: forall t f es a.
    (Pointed f) => Eff (Lit3 f t : es) a -> Eff es a
runLit3' = interpret_ \case Lit3 value -> pure (point value)

-- ** Unbound

data Unbound (sym :: Symbol) f t :: Effect where
    Unbound :: Unbound sym f t m (f t)

makeEffect ''Unbound

runUnbound :: forall (sym :: Symbol) f t es a.
    (f t) -> Eff (Unbound sym f t : es) a -> Eff es a
runUnbound value = interpret_ \case Unbound -> pure value

runUnbound' :: forall (sym :: Symbol) f t es a.
    (Pointed f) => t -> Eff (Unbound sym f t : es) a -> Eff es a
runUnbound' value = interpret_ \case Unbound -> pure $ point value

-- ** Field

data Field f :: Effect where
    Field :: (KnownSymbol k, HasField k r t) => m (f r) -> Field f m (f t)

makeEffect ''Field

instance (KnownSymbol k, HasField k r t, Field f :> es)
    => HasField k (Eff' es f r) (Eff' es f t) where
    getField r = field @k r

getField :: forall k r t es f.
    (KnownSymbol k, HasField k r t, Field f :> es)
    => S' es f (r -> Ret t)
getField r = field @k r

runField :: forall f es fullEs a.
    (fullEs ~ Field f : es)
    => (forall k r t. (KnownSymbol k, HasField k r t) => f r -> Eff' fullEs f t)
    -> Eff fullEs a -> Eff es a
runField getField = interpret $ \env -> \case
    Field @k r -> localSeqBorrow @'[Field f] env $ \borrow ->
        borrow $ localSeqUnlift env (\unlift -> unlift r) >>= getField @k

-- ** Morphisms

data MorphBase (sym :: Symbol) a b :: Effect where
    Morph :: forall sym a b m.
        m a -> MorphBase sym a b m b

data MorphBase2 (sym :: Symbol) a b c :: Effect where
    Morph2 :: forall sym a b c m.
        m a -> m b -> MorphBase2 sym a b c m c

data MorphBase3 (sym :: Symbol) a b c d :: Effect where
    Morph3 :: forall sym a b c d m.
        m a -> m b -> m c -> MorphBase3 sym a b c d m d

data MorphBase4 (sym :: Symbol) a b c d e :: Effect where
    Morph4 :: forall sym a b c d e m.
        m a -> m b -> m c -> m d -> MorphBase4 sym a b c d e m e

makeEffect ''MorphBase
makeEffect ''MorphBase2
makeEffect ''MorphBase3
makeEffect ''MorphBase4

type Morph (sym :: Symbol) f a b = MorphBase sym (f a) (f b)
type Morph2 (sym :: Symbol) f a b c = MorphBase2 sym (f a) (f b) (f c)
type Morph3 (sym :: Symbol) f a b c d = MorphBase3 sym (f a) (f b) (f c) (f d)
type Morph4 (sym :: Symbol) f a b c d e = MorphBase4 sym (f a) (f b) (f c) (f d) (f e)

runMorph :: forall (sym :: Symbol) a b es fullEs n.
    (fullEs ~ MorphBase sym a b : es)
    => (Eff fullEs a -> Eff fullEs b)
    -> Eff fullEs n -> Eff es n
runMorph f = interpret $ \env -> \case
    Morph a -> localSeqBorrow @'[MorphBase sym a b] env
        \borrow -> borrow $
            localSeqUnlift env \unlift -> f (unlift a)

runMorph2 :: forall (sym :: Symbol) a b c es fullEs n.
    (fullEs ~ MorphBase2 sym a b c : es)
    => (Eff fullEs a -> Eff fullEs b -> Eff fullEs c)
    -> Eff fullEs n -> Eff es n
runMorph2 f = interpret $ \env -> \case
    Morph2 a b -> localSeqBorrow @'[MorphBase2 sym a b c] env
        \borrow -> borrow $
            localSeqUnlift env \unlift -> f (unlift a) (unlift b)

runMorph3 :: forall (sym :: Symbol) a b c d es fullEs n.
    (fullEs ~ MorphBase3 sym a b c d : es)
    => (Eff fullEs a -> Eff fullEs b -> Eff fullEs c -> Eff fullEs d)
    -> Eff fullEs n -> Eff es n
runMorph3 f = interpret $ \env -> \case
    Morph3 a b c -> localSeqBorrow @'[MorphBase3 sym a b c d] env
        \borrow -> borrow $
            localSeqUnlift env \unlift -> f (unlift a) (unlift b) (unlift c)

runMorph4 :: forall (sym :: Symbol) a b c d e es fullEs n.
    (fullEs ~ MorphBase4 sym a b c d e : es)
    => (Eff fullEs a -> Eff fullEs b -> Eff fullEs c -> Eff fullEs d -> Eff fullEs e)
    -> Eff fullEs n -> Eff es n
runMorph4 f = interpret $ \env -> \case
    Morph4 a b c d -> localSeqBorrow @'[MorphBase4 sym a b c d e] env
        \borrow -> borrow $
            localSeqUnlift env \unlift -> f (unlift a) (unlift b) (unlift c) (unlift d)