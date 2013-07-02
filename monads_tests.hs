{--| Tests for Monads Tutorial

James Wang, 25 Jun 2013

--}

module Main (
  main
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Test.HUnit

import Monads

main :: IO ()
main = defaultMain tests


exercise1test :: Float -> Bool
exercise1test x = (bind' f $ g x) == ((x / 2.0) + 3.0, "g was called; f was called; ")

unittestFUnit :: Float -> Bool
unittestFUnit x = (bind' f $ unit x) == (x + 3.0, "f was called; ")

unittestUnitF :: Float -> Bool
unittestUnitF x = (bind' unit $ f x) == (x + 3.0, "f was called; ")

testliftprop :: Test.HUnit.Test
testliftprop =
  "lift test" Test.HUnit.~: (bind' (lift z) $ (lift q) 3.0) Test.HUnit.@=? (lift (z . q) 3.0)
  where z = \x -> x + 4.0
        q = \x -> x - 1.0


tests :: [Test]
tests =
  [
    testGroup "Exercise 1"
    [
      testProperty "Bind applies correctly" exercise1test
    ],
    testGroup "Exercise 2"
    [
      testProperty "f id = f" unittestFUnit,
      testProperty "id f = f" unittestUnitF
    ],
    testGroup "Exercise 3"
    [
      testGroup "HUnit tests for lift f * lift g = (lift f.g)" $ hUnitTestToTests testliftprop
    ]
  ]
      
