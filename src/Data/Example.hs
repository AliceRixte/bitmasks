



module Data.Example where

import Data.Word (Word8)
import Data.Bitmask



data PizzaTopping =
    Cheese
  | Mushrooms
  | Pineapple
  | Ham
  deriving (Show, Eq, Bounded, Enum)


type PizzaMask = Bitmask PizzaTopping Word8


-- A Margherita pizza (cheese only)
margherita :: PizzaMask
margherita = fromFlags [Cheese]

-- A veggie pizza (everything except ham)
veggie :: PizzaMask
veggie = exceptFlags [Ham]

-- Add multiple toppings to a pizza:
hawaiian :: PizzaMask
hawaiian = addFlags [Pineapple, Ham] margherita

-- Mask a pizza with another (bitwise AND):
veggieHawaiian :: PizzaMask
veggieHawaiian = veggie .&. hawaiian

-- Flip the presence of toppings on a pizza:
funghi :: PizzaMask
funghi = flipFlags [Pineapple, Mushrooms] veggieHawaiian

-- Remove a topping and compare to Margherita:
margheritaAgain :: Bool
margheritaAgain = margherita == deleteFlag Mushrooms funghi


-- | Example queries (doctest style)

-- >>> getFlag Cheese funghi
-- True
-- >>> getFlag Pineapple funghi
-- False
-- >>> getFlags [Pineapple, Mushrooms] hawaiian
-- [True,False]
-- >>> getFlag Ham veggieHawaiian
-- False
-- >>> toFlags funghi
-- [Cheese,Mushrooms]
-- >>> margheritaAgain
-- True
-- >>> toFlags funghi
-- [Cheese,Mushrooms]
-- >>> toFlags hawaiian
-- [Cheese,Pineapple,Ham]
-- >>> toFlagsBool funghi
-- [(Cheese,True),(Mushrooms,True),(Pineapple,False),(Ham,False)]

