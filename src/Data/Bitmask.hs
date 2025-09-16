module Data.Bitmask where

import Data.Bits

newtype Bitmask flag w = Bitmask { unBitmask :: w }

-- | Get a flag from a bitmask.
--
getFlag :: (Bits w, Num w, Enum flag) => flag -> Bitmask flag w -> Bool
getFlag flag (Bitmask w) = testBit w (fromEnum flag)

-- | Set a flag in a bitmask.
--
setFlag :: (Bits w, Num w, Enum flag) =>
  flag -> Bool -> Bitmask flag w -> Bitmask flag w
setFlag flag value (Bitmask w) = Bitmask $
  if value then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

-- | Flip a flag in a bitmask.
--
flipFlag :: (Bits w, Num w, Enum flag) =>
  flag -> Bitmask flag w -> Bitmask flag w
flipFlag flag (Bitmask w) = Bitmask $ complementBit w (fromEnum flag)

-- | Modify a flag in a bitmask.
modifyFlag :: (Bits w, Num w, Enum flag) =>
  flag -> (Bool -> Bool) -> Bitmask flag w -> Bitmask flag w
modifyFlag flag f (Bitmask w) = Bitmask $
  if f (testBit w (fromEnum flag)) then
    setBit w (fromEnum flag)
  else
    clearBit w (fromEnum flag)

