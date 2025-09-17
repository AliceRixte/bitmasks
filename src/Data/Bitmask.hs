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
-- Bitmasks for efficient storing of boolean flags
--
--------------------------------------------------------------------------------

module Data.Bitmask
  ( Bitmask
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
  -- ** Re-exports from "Data.Bits"
  , (.&.)
  , (.|.)
  , xor
  , complement
  )
  where

import Data.Bits
import Data.Bitmask.Internal

