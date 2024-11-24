-- | Pairings between functors.
-- |
-- | Based on <http://hackage.haskell.org/package/adjunctions-0.6.0/docs/Data-Functor-Zap.html>.

module Data.Functor.Pairing
  ( Pairing
  , type (⋈)
  , zap
  , sym
  , identity
  , productCoproduct
  , stateStore
  , readerEnv
  , writerTraced
  , freeCofree
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, explore)
import Control.Comonad.Cofree.Trans (CofreeT, runCofreeT)
import Control.Comonad.Env.Trans (EnvT(..))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Control.Monad.Free (Free)
import Control.Monad.Free.Trans (FreeT, resume)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Data.Maybe
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import React.DOM.Dynamic (a)

-- | A pairing between functors `f` and `g`.
-- |
-- | This asserts that any sums in `f` can annihilate any products in `g`, and vice
-- | versa.
-- |
-- | This library provides some useful pairings, and ways of lifting pairings over
-- | various constructions on `Functor`s.
type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

infix 4 type Pairing as ⋈

zap :: forall f g a b. f ⋈ g -> f (a -> b) -> g a -> b
zap pairing = pairing ($)

-- | Pairing is symmetric
sym :: forall f g. f ⋈ g -> g ⋈ f
sym pairing f ga fb = pairing (flip f) fb ga

-- | The identity functor pairs with itself
identity :: Identity ⋈ Identity
identity f (Identity a) (Identity b) = f a b

-- instance pairTupleFunction :: Pairing (Tuple a) ((->) a) where
--   pair f (Tuple a b) g = f b (g a)

-- maybeWat :: forall a. Maybe a ⋈ Identity
-- maybeWat f (Just a) g = 
-- maybeWat f (Nothing) g = 

type Stack = Cofree Maybe -- We can't pair Maybe. Can we pair Cofree Maybe? 

-- data Cofree f a = a :< f (Cofree f a)

-- data Cofree Maybe a
--      = a :< Just (Cofree Maybe a)
--      | a :< Nothing
     
-- stack :: Cofree Maybe Char
-- stack = 'a' :< Just ('b' :< Just ('c' :< Nothing))

-- stackWat :: forall a. Cofree (Maybe a) ⋈ g 
-- stackWat f (a :< _) (Identity b) = f a b


-- I think if we can write a typelevel Cofree' n witnessing a certain
-- stack size then we can pair it with a Fin n.
-- stackWat2 :: forall a (n :: Nat). Cofree' n (Maybe a) ⋈ Fin n
-- ????

-- type NSum a = Either a (NSum a)

-- data Free f a
--   = Pure a
--   | Free (f (Free f a))

-- data Free Identity a
--   = Pure a
--   | Free (Identity (Free Identity a))

-- type NSum' a = Free Identity a -- Nat
-- nsum :: NSum' Int
-- nsum = Free (Identity (Pure 0)) -- Pure 0

type Stream a = Cofree Identity a -- Stream a

-- Pairing Nat with Stream a, means picking the nth element of the stream.

-- data Cofree Identity a
--      = a :< Identity (Cofree Identity a)

-- blah :: Blah Int
-- blah = 0 :< Identity (1 :< Identity (...))

-- stackWat3 :: forall a. Cofree (Maybe a) ⋈ Nat
-- stackWat3 f (a :< Nothing) (Identity b) = f a b
-- stackWat3 f (a :< _) (Identity b) = f a b

-- with a dependently typed Cofree (Maybe a) we could pair it with Fin n?

tupleFunction :: forall a. Tuple a ⋈ ((->) a) 
tupleFunction f (Tuple a b) g = f b (g a)

-- | Functor products pair with functor coproducts
productCoproduct :: forall f1 g1 f2 g2. f1 ⋈ g1 -> f2 ⋈ g2 -> Product f1 f2 ⋈ Coproduct g1 g2
productCoproduct p1 p2 f (Product (Tuple f1 f2)) (Coproduct e) =
  case e of
    Left g1 -> p1 f f1 g1
    Right g2 -> p2 f f2 g2

-- | `StateT` pairs with `StoreT`.
stateStore :: forall f g s. f ⋈ g -> StateT s f ⋈ StoreT s g
stateStore pairing f (StateT state) (StoreT (Tuple gf s)) =
  pairing (\(Tuple a s1) f1 -> f a (f1 s1)) (state s) gf

-- | `ReaderT` pairs with `EnvT`.
readerEnv :: forall f g e. f ⋈ g -> ReaderT e f ⋈ EnvT e g
readerEnv pairing f (ReaderT reader) (EnvT (Tuple e gb)) =
  pairing f (reader e) gb

-- | `WriterT` pairs with `TracedT`.
writerTraced :: forall f g w. f ⋈ g -> WriterT w f ⋈ TracedT w g
writerTraced pairing f (WriterT writer) (TracedT gf) =
  pairing (\(Tuple a w) f1 -> f a (f1 w)) writer gf

-- | `Free` pairs with `Cofree`.
freeCofree :: forall f g. Functor f => Functor g => f ⋈ g -> Free f ⋈ Cofree g
freeCofree pairing f = explore (zap pairing) <<< map f

-- instance pairFreeCofree :: (Functor f, Functor g, Pairing f g) => Pairing (Free f) (Cofree g) where
--   pair f = explore zap <<< map f

-- | `FreeT` pairs with `CofreeT`.
freeTCofreeT :: forall f g m w. MonadRec m => Comonad w => Functor f => Functor g => f ⋈ g -> m ⋈ w -> FreeT f m ⋈ CofreeT g w
freeTCofreeT p1 p2 f m w = go (step m w)
    where
      go (Tuple mf wc) =
        case p2 Tuple mf wc of
          Tuple (Left a) (Tuple b _) -> f a b
          Tuple (Right ff) (Tuple _ tail) -> go (p1 step ff tail)

      -- step :: forall a. FreeT f m a -> CofreeT g w a -> Tuple (m (Either a (f (FreeT f m a)))) (w (Tuple a (g (CofreeT g w a))))
      step a b = Tuple (resume a) (runCofreeT b)