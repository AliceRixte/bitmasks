{-# LANGUAGE GHC2021#-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Bitmask
-- Description :  Bitmasks
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  portable
--
-- Bitmasks for boolean flags.
--
-- == Usage
--
-- @
-- data MyFlags =
--     FlagA
--   | FlagB
--   | FlagC
-- deriving (Show, Eq, Bounded, Enum)
--
-- type MyBitmask = Bitmask MyFlags Word8
--
-- @
--
-- >>> import Data.Word
-- >>> data MyFlags = FlagA | FlagB | FlagC deriving (Show, Eq, Bounded, Enum)
--
-- >>> checkBitmask (noFlag :: Bitmask MyFlags Word8)
-- True
--
--
-- >>> type MyBitmask = Bitmask MyFlags Word8
--
-- >>> getFlag FlagA (noFlag :: MyBitmask)
-- False
--
-- >>> getFlag FlagA (allFlags :: MyBitmask)
-- True
-- >>> getFlag FlagA (setFlag FlagA True noFlag)
-- True
--
-- >>> getFlags [FlagA, FlagB] (fromFlags [FlagA] :: MyBitmask)
-- [True,False]
--
--------------------------------------------------------------------------------

module Data.Bitmask
  ( module Data.Bitmask.Internal
  -- ** Re-exports from "Data.Bits"
  , module Data.Bits
  )
  where

import Data.Bits
import Data.Bitmask.Internal

