{-# LANGUAGE GHC2021#-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Bitmask.Internal
-- Description :  Bitmasks
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  portable
--
--  Bitmasks for efficient storing of boolean flags
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
--------------------------------------------------------------------------------

module Data.Bitmask.Internal
  ( Bitmask(..)
  , Bitmask8
  , Bitmask16
  , Bitmask32
  , Bitmask64
  -- ** Conversion to and from bits
  , fromBits
  , toBits
  -- ** Check bitmask validity
  , checkBitmask
  -- ** Bitmask creation
  , noFlag
  , allFlags
  , fromFlags
  , toFlags
  , fromExceptFlags
  , toExceptFlags
  , fromFlagsBool
  , toFlagsBool
  -- ** Flag querying
  , getFlag
  , getFlags
  -- ** Flag modification
  , addFlag
  , addFlags
  , deleteFlag
  , deleteFlags
  , flipFlag
  , flipFlags
  , setFlag
  , setFlags
  , modifyFlag
  , modifyFlags
  ) where

import Data.Word
import Data.Bits


-- | A bitmask that contains boolean flags
--
-- * The 'flag' type should be an enumeration type (i.e. an instance of 'Enum').
--
-- * The 'w' type should be an integral type (e.g. 'Word8', 'Word32', etc.) that
-- supports bitwise operations.
--
-- * The number of bits in 'w' must be at least as many as the number of
-- constructors in 'flag'.
--
-- [Usage:]
--
-- @
-- data PizzaTopping =
--    Cheese
--  | Mushrooms
--  | Pineapple
--  | Ham
--  deriving (Show, Eq, Bounded, Enum)
--
--  type PizzaMask = Bitmask8 PizzaTopping Word8
-- @
--
newtype Bitmask w flag = Bitmask w
  deriving (Eq, Ord, Show, Bits)

type Bitmask8 = Bitmask Word8
type Bitmask16 = Bitmask Word16
type Bitmask32 = Bitmask Word32
type Bitmask64 = Bitmask Word64

-- | Create a bitmask from raw bits.
--
fromBits :: w -> Bitmask w flag
fromBits = Bitmask

-- | Convert a bitmask to raw bits.
--
toBits :: Bitmask w flag -> w
toBits (Bitmask w) = w

-- | Check that a bitmask can represent all flags.
--
-- >>> checkBitmask (allFlags :: Bitmask8 PizzaTopping)
-- True
--
checkBitmask :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask w flag -> Bool
checkBitmask (Bitmask w) =
  finiteBitSize w >= (fromEnum (maxBound :: flag) + 1)

---------------------- Creation and conversion to lists ----------------------

-- | A bitmask with all flags set to 'False'.
--
-- >>> getFlag Mushrooms (noFlag :: PizzaMask)
-- False
--
noFlag :: Bits w => Bitmask w flag
noFlag = Bitmask zeroBits

-- | A bitmask with all flags set to 'True'.
--
-- >>> getFlag Mushrooms (allFlags :: PizzaMask)
-- True
--
allFlags :: (FiniteBits w, Enum flag) => Bitmask w flag
allFlags = Bitmask oneBits

-- | Create a bitmask from a list of flags to set to 'True'.
--
-- >>> hawaiian = fromFlags [Pineapple, Ham, Cheese] :: PizzaMask
--
fromFlags :: (Bits w, Enum flag) => [flag] -> Bitmask w flag
fromFlags = foldr addFlag noFlag

-- | Convert a bitmask to a list of flags that are set to 'True'.
--
-- >>> toFlags hawaiian
-- [Cheese,Pineapple,Ham]
--
toFlags :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask w flag -> [flag]
toFlags bm@(Bitmask w) =
  let n = finiteBitSize w in
  filter (`getFlag` bm)
    [toEnum i | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

-- | Create a bitmask from a list of flags to set to 'False'
--
-- >>> veggie = fromExceptFlags [Ham] :: PizzaMask
--
fromExceptFlags :: (FiniteBits w, Enum flag) => [flag] -> Bitmask w flag
fromExceptFlags = foldr deleteFlag allFlags

-- | Convert a bitmask to a list of flags that are set to 'False'.
--
-- >>> toExceptFlags veggie
-- [Ham]
toExceptFlags :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask w flag -> [flag]
toExceptFlags bm@(Bitmask w) =
  let n = finiteBitSize w in
  filter (not . (`getFlag` bm))
    [toEnum i | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

-- | Convert an association list of flags and boolean values to a bitmask.
--
-- >>> funghi = fromFlagsBool [(Cheese, True), (Ham, False), (Mushrooms, True)] :: PizzaMask
fromFlagsBool :: forall flag w. (Bits w, Enum flag)
  => [(flag, Bool)] -> Bitmask w flag
fromFlagsBool = foldr (uncurry setFlag) noFlag

-- | Convert a bitmask to an association list of flags and boolean values.
--
-- >>> toFlagsBool funghi
-- [(Cheese,True),(Mushrooms,True),(Pineapple,False),(Ham,False)]
--
toFlagsBool :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask w flag -> [(flag, Bool)]
toFlagsBool bm@(Bitmask w) =
  let n = finiteBitSize w in
  [(toEnum i, getFlag (toEnum i) bm)
    | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

---------------------- Querying and modifying flags ----------------------

-- | Get a flag from a bitmask.
--
-- >>> getFlag Mushrooms hawaiian
-- False
--
getFlag :: (Bits w, Enum flag) => flag -> Bitmask w flag -> Bool
getFlag flag (Bitmask w) = testBit w (fromEnum flag)

-- | Get multiple flags from a bitmask.
--
-- >>> getFlags [Cheese, Mushrooms] hawaiian
-- [True,False]
--
getFlags :: (Bits w, Enum flag) => [flag] -> Bitmask w flag -> [Bool]
getFlags fs bm = map (`getFlag` bm) fs

-- | Add a flag to a bitmask (set it to 'True').
--
-- >>> margherita = addFlag Cheese (noFlag :: PizzaMask)
--
addFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask w flag -> Bitmask w flag
addFlag f = setFlag f True

-- | Add multiple flags to a bitmask (set them to 'True').
--
-- >>> hawaiian = addFlags [Pineapple, Ham] margherita
--
addFlags :: (Bits w, Enum flag)
  => [flag] -> Bitmask w flag -> Bitmask w flag
addFlags fs bm = foldr addFlag bm fs

-- | Remove a flag from a bitmask (set it to 'False').
--
-- >>> veggie = deleteFlag Ham (allFlags :: PizzaMask)
--
deleteFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask w flag -> Bitmask w flag
deleteFlag f = setFlag f False

-- | Remove multiple flags from a bitmask (set them to 'False').
--
-- >>> picky = deleteFlags [Pineapple, Ham] (allFlags :: PizzaMask)
--
deleteFlags :: (Bits w, Enum flag)
  => [flag] -> Bitmask w flag -> Bitmask w flag
deleteFlags fs bm = foldr deleteFlag bm fs

-- | Set a flag in a bitmask.
--
-- >>> funghi = setFlag Mushrooms True margherita
setFlag :: (Bits w, Enum flag) =>
  flag -> Bool -> Bitmask w flag -> Bitmask w flag
setFlag flag value (Bitmask w) = Bitmask $
  if value then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

-- | Set multiple flags in a bitmask.
--
-- >>> hawaiian = setFlags [(Ham, True), (Pineapple, True)] margherita
--
setFlags :: (Bits w, Enum flag) =>
  [(flag, Bool)] -> Bitmask w flag -> Bitmask w flag
setFlags fs bm = foldr (uncurry setFlag) bm fs

-- | Flip a flag in a bitmask.
--
-- >>> margherita = flipFlag Cheese (noFlag :: PizzaMask)
flipFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask w flag -> Bitmask w flag
flipFlag flag (Bitmask w) = Bitmask $ complementBit w (fromEnum flag)

-- | Flip multiple flags in a bitmask.
--
-- >>> funghi = flipFlags [Mushrooms, Ham, Pineapple] hawaiian
flipFlags :: (Bits w, Enum flag) =>
  [flag] -> Bitmask w flag -> Bitmask w flag
flipFlags fs bm = foldr flipFlag bm fs

-- | Modify a flag in a bitmask.
--
-- >>> veggie = modifyFlag Ham not (allFlags :: PizzaMask)
--
modifyFlag :: (Bits w, Enum flag) =>
  flag -> (Bool -> Bool) -> Bitmask w flag -> Bitmask w flag
modifyFlag flag f (Bitmask w) = Bitmask $
  if f (testBit w (fromEnum flag)) then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

-- | Modify multiple flags in a bitmask.
--
-- >>> picky = modifyFlags [Pineapple, Ham] not (allFlags :: PizzaMask)
--
modifyFlags :: (Bits w, Enum flag) =>
  [flag] -> (Bool -> Bool) -> Bitmask w flag -> Bitmask w flag
modifyFlags fs f bm = foldr (`modifyFlag` f) bm fs
