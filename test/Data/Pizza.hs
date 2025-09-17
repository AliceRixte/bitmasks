{-# LANGUAGE DerivingVia #-}

module Data.Pizza where

import Data.Bitmask

import Test.QuickCheck

-- Pizza toppings enumeration

data PizzaTopping =
    Cheese
  | Mushrooms
  | Pineapple
  | Ham
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Arbitrary PizzaTopping where
  arbitrary = chooseEnum (minBound :: PizzaTopping, maxBound :: PizzaTopping)

type PizzaMask = Bitmask8 PizzaTopping

-- Pizza examples
margherita :: PizzaMask
margherita = fromFlags [Cheese]

veggie :: PizzaMask
veggie = fromExceptFlags [Ham]

hawaiian :: PizzaMask
hawaiian = addFlags [Pineapple, Ham] margherita
