module Test.Main where


import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree, head, tail)
import Control.Monad.Free.Class (wrapFree)
import Data.Functor.Pairing.Co (Co, co)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Foldable, class Traversable, foldlDefault, foldrDefault, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Explore (Component, UI, explore)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type CounterModel = { count :: Int }

newtype CounterInterface a = CounterInterface
  { changeCount :: Int -> a
  , increment :: a
  , get :: Int /\ a
  }
derive instance functorCounter :: Functor CounterInterface

-- instance foldableCounter :: Foldable CounterInterface where
--   -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
--   foldMap f (CounterInterface {get}) = f get
--   foldr f = foldrDefault f
--   foldl f = foldlDefault f

-- instance traversableCounter :: Traversable CounterInterface where
--   -- traverse :: forall a b m. Distributive m => Monad m => (a -> m b) -> CounterInterface a -> m (CounterInterface b)
--   traverse f (CounterInterface {changeCount, increment, get}) =
--     (\g i c -> CounterInterface {get: g, increment: i, changeCount: c})
--       <$> f get
--       <*> f increment
--       <*> traverse f changeCount
--   sequence = sequenceDefault


type Counter = Cofree CounterInterface

-- change :: Int -> Co CounterInterface Unit
-- change count = coT (\(CounterInterface { changeCount }) -> changeCount count $ unit)

-- This really only selects the function that will be ran in the interpreter.
inc :: Co CounterInterface Unit
inc = co \(CounterInterface { increment }) ->  increment $ unit

get :: Co CounterInterface CounterModel
get = co \(CounterInterface { get: (i /\ f) }) -> f $ { count : i }

-- co :: ∀ w a. Functor w ⇒ (∀ r. w (a → r) → r) → Co w a

-- inc' :: Co Counter Unit
-- inc' = co \cof ->
--   let
--     (CounterInterface {increment}) = tail cof
--   in
--     head increment $ unit

-- changeCount :: Int -> Co CounterInterface Unit
-- changeCount i = co \(CounterInterface { changeCount }) -> changeCount $ Tuple i unit

-- changeCount :: Int -> Co CounterInterface Unit
-- changeCount i = co \cof ->
--   let (CounterInterface {changeCount}) = tail cof
--   in
--     head changeCount Tuple i unit

unfold :: ∀ f s a. Functor f ⇒ (s → a) → (s → f s) → s → Cofree f a
unfold e n s = deferCofree \_ -> Tuple (e s) (map (unfold e n) $ n s)


counter :: Component Counter Props
counter = unfold render interpreter $ {count: 0}

  where

    -- The concrete comonad
    interpreter :: CounterModel -> CounterInterface CounterModel
    interpreter {count} = CounterInterface
      { changeCount: \c -> { count: c }
      , increment: { count: add 1 count }
      , get : count /\ { count }
      }

    render :: CounterModel -> UI (Co Counter Unit) Props
    render { count } { label } send = do
      R.div_
        [ R.h1
            { tabIndex:  0
            , onKeyUp: handler targetValue
                          \_ -> send (wrapFree $ inc $> pure unit)
            , onClick: handler targetValue
                          \_ -> do
                            val <- send (wrapFree $ get $> pure unit)
                            log $ "click: " <> show count
                            send (wrapFree $ inc $> pure unit)
            , children: [ R.text ( label <> " " <> show count) ]
            }
        ]

type Props =
  { label :: String
  }

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      root <- createRoot c
      renderRoot root ((explore counter) { label: "Counter"})
