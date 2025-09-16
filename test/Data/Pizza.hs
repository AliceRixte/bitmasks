{-# LANGUAGE DerivingVia #-}

module Data.Pizza where

import Data.Word (Word8)
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

type PizzaMask = Bitmask PizzaTopping Word8

-- Pizza examples
margherita :: PizzaMask
margherita = fromFlags [Cheese]

veggie :: PizzaMask
veggie = exceptFlags [Ham]

hawaiian :: PizzaMask
hawaiian = addFlags [Pineapple, Ham] margherita
