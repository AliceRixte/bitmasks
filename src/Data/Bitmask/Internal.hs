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
-- Bitmasks for boolean flags.
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

module Data.Bitmask.Internal where

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
--
newtype Bitmask flag w = Bitmask w
  deriving (Eq, Ord, Show, Bits)

-- | Check that a bitmask can represent all flags.
--
-- >>> checkBitmask (noFlag :: Bitmask MyFlags Word8)
-- True
--
checkBitmask :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask flag w -> Bool
checkBitmask (Bitmask w) =
  finiteBitSize w >= (fromEnum (maxBound :: flag) + 1)

-- | A bitmask with all flags set to 'False'.
--
-- >>> getFlag FlagA (noFlag :: MyBitmask)
-- False
--
noFlag :: Bits w => Bitmask flag w
noFlag = Bitmask zeroBits

-- | A bitmask with all flags set to 'True'.
--
-- >>> getFlag FlagA (allFlags :: MyBitmask)
-- True
--
allFlags :: (FiniteBits w, Enum flag) => Bitmask flag w
allFlags = Bitmask oneBits

-- | Create a bitmask from a list of flags to set to 'True'.
--
flags :: (Bits w, Enum flag) => [flag] -> Bitmask flag w
flags = foldr addFlag noFlag

-- | Same as 'flags'.
--
fromFlags :: (Bits w, Enum flag) => [flag] -> Bitmask flag w
fromFlags = flags

-- | Convert a bitmask to a list of flags that are set to 'True'.
--
toFlags :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask flag w -> [flag]
toFlags bm@(Bitmask w) =
  let n = finiteBitSize w in
  filter (`getFlag` bm)
    [toEnum i | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

-- | Create a bitmask from a list of flags to set to 'False'
--
exceptFlags :: (FiniteBits w, Enum flag) => [flag] -> Bitmask flag w
exceptFlags = foldr deleteFlag allFlags

-- | Convert a bitmask to a list of flags that are set to 'False'.
toExceptFlags :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask flag w -> [flag]
toExceptFlags bm@(Bitmask w) =
  let n = finiteBitSize w in
  filter (not . (`getFlag` bm))
    [toEnum i | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

-- | Convert an association list of flags and boolean values to a bitmask.
--
fromFlagsBool :: forall flag w. (Bits w, Enum flag)
  => [(flag, Bool)] -> Bitmask flag w
fromFlagsBool = foldr (uncurry setFlag) noFlag

-- | Convert a bitmask to an association list of flags and boolean values.
--
toFlagsBool :: forall flag w. (FiniteBits w, Enum flag, Bounded flag)
  => Bitmask flag w -> [(flag, Bool)]
toFlagsBool bm@(Bitmask w) =
  let n = finiteBitSize w in
  [(toEnum i, getFlag (toEnum i) bm)
    | i <- [0 .. min (n - 1) (fromEnum (maxBound :: flag))]]

-- | Get a flag from a bitmask.
--
-- >>> getFlag FlagA allFlags
-- True
--
getFlag :: (Bits w, Enum flag) => flag -> Bitmask flag w -> Bool
getFlag flag (Bitmask w) = testBit w (fromEnum flag)

-- | Get multiple flags from a bitmask.
--
getFlags :: (Bits w, Enum flag) => [flag] -> Bitmask flag w -> [Bool]
getFlags fs bm = map (`getFlag` bm) fs

-- | Add a flag to a bitmask (set it to 'True').
--
addFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask flag w -> Bitmask flag w
addFlag f = setFlag f True

-- | Add multiple flags to a bitmask (set them to 'True').
--
addFlags :: (Bits w, Enum flag)
  => [flag] -> Bitmask flag w -> Bitmask flag w
addFlags fs bm = foldr addFlag bm fs

-- | Remove a flag from a bitmask (set it to 'False').
--
deleteFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask flag w -> Bitmask flag w
deleteFlag f = setFlag f False

-- | Remove multiple flags from a bitmask (set them to 'False').
--
deleteFlags :: (Bits w, Enum flag)
  => [flag] -> Bitmask flag w -> Bitmask flag w
deleteFlags fs bm = foldr deleteFlag bm fs

-- | Set a flag in a bitmask.
--
setFlag :: (Bits w, Enum flag) =>
  flag -> Bool -> Bitmask flag w -> Bitmask flag w
setFlag flag value (Bitmask w) = Bitmask $
  if value then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

-- | Set multiple flags in a bitmask.
--
setFlags :: (Bits w, Enum flag) =>
  [(flag, Bool)] -> Bitmask flag w -> Bitmask flag w
setFlags fs bm = foldr (uncurry setFlag) bm fs

-- | Flip a flag in a bitmask.
--
flipFlag :: (Bits w, Enum flag) =>
  flag -> Bitmask flag w -> Bitmask flag w
flipFlag flag (Bitmask w) = Bitmask $ complementBit w (fromEnum flag)

-- | Flip multiple flags in a bitmask.
--
flipFlags :: (Bits w, Enum flag) =>
  [flag] -> Bitmask flag w -> Bitmask flag w
flipFlags fs bm = foldr flipFlag bm fs

-- | Modify a flag in a bitmask.
--
modifyFlag :: (Bits w, Enum flag) =>
  flag -> (Bool -> Bool) -> Bitmask flag w -> Bitmask flag w
modifyFlag flag f (Bitmask w) = Bitmask $
  if f (testBit w (fromEnum flag)) then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

-- | Modify multiple flags in a bitmask.
--
modifyFlags :: (Bits w, Enum flag) =>
  [flag] -> (Bool -> Bool) -> Bitmask flag w -> Bitmask flag w
modifyFlags fs f bm = foldr (`modifyFlag` f) bm fs
