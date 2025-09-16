{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitmaskSpec where

import Data.List (group, sort)

import Test.Hspec
import Test.QuickCheck

import Data.Bitmask.Internal hiding (flags)
import Data.Pizza

instance Arbitrary w => Arbitrary (Bitmask flag w) where
  arbitrary = Bitmask <$> arbitrary

rmDups :: (Ord a) => [a] -> [a]
rmDups = map head . group . sort

getAll :: Property
getAll = property $ \(flag :: PizzaTopping) ->
  getFlag flag (allFlags :: PizzaMask) == True

getNone :: Property
getNone = property $ \(flag :: PizzaTopping) ->
  getFlag flag (noFlag :: PizzaMask) == False

fromToFlags :: Property
fromToFlags = property $ \(flags :: [PizzaTopping])->
  toFlags (fromFlags flags :: PizzaMask) == rmDups flags

toFromFlags :: Property
toFromFlags = property $ \bm@(Bitmask w :: PizzaMask) ->
  w >= 16 || fromFlags (toFlags bm) == bm

fromToExcept :: Property
fromToExcept = property $ \(flags :: [PizzaTopping])->
  toExceptFlags (exceptFlags flags :: PizzaMask) == rmDups flags

toFromExcept :: Property
toFromExcept = property $ \bm@(Bitmask w :: PizzaMask) ->
  w >= 16 || exceptFlags (toExceptFlags bm) == bm

fromToFlagsBool :: Property
fromToFlagsBool = property $ \(flagsBool :: [(PizzaTopping, Bool)]) ->
  toFlagBools (fromFlagsBool flagsBool :: PizzaMask) == rmDups flagsBool

getAdd :: Property
getAdd = property $ \(flag :: PizzaTopping) (bm :: PizzaMask) ->
  getFlag flag (addFlag flag bm) == True

getAdds :: Property
getAdds = property $ \(flags :: [PizzaTopping]) (bm :: PizzaMask) ->
  all id $ getFlags flags (addFlags flags bm)

getDelete :: Property
getDelete = property $ \(flag :: PizzaTopping) (bm :: PizzaMask) ->
  getFlag flag (deleteFlag flag bm) == False

getDeletes :: Property
getDeletes = property $ \(flags :: [PizzaTopping]) (bm :: PizzaMask) ->
  all not $ getFlags flags (deleteFlags flags bm)


flipModifs :: Property
flipModifs = property $ \(flags :: [PizzaTopping]) (bm :: PizzaMask) ->
  modifyFlags flags not bm == flipFlags flags bm

spec :: Spec
spec = do
  describe "Bitmask" $ do
    it "checkBitmask allFlags == True" $ property $
      checkBitmask (allFlags :: PizzaMask)
    it "fromToFlags" $ property fromToFlags
    it "toFromFlags" $ property toFromFlags
    it "fromToExcept" $ property fromToExcept
    it "toFromExcept" $ property toFromExcept
    it "getFlag flag allFlags == True" $ property getAll
    it "getFlag flag noFlag == False" $ property getNone
    it "getFlag flag (addFlag flag bm) == True" $ property getAdd
    it "all id (getFlags flags (addFlags flags bm))" $ property getAdds
    it "getFlag flag (deleteFlag flag bm) == False" $ property getDelete
    it "all not (getFlags flags (deleteFlags flags bm))" $ property getDeletes
    it "modifyFlags not flags bm == flipFlags flags bm" $ property flipModifs