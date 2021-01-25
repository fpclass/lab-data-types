--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Data types                                                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Tasty.QuickCheck.Laws.Eq

import qualified Lab as L

--------------------------------------------------------------------------------

instance Arbitrary L.IntPos where 
    arbitrary = L.IntPos <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (L.Pos a) where
    arbitrary = L.Pos <$> arbitrary <*> arbitrary

tests :: TestTree 
tests = testGroup "" 
 [
     testEqLaws (Proxy @L.IntPos)
 ,   testProperty "Show instance for IntPos works" $ 
        \(x :: Int) -> \(y :: Int) ->
        show (L.IntPos x y) === concat ["IntPos ", show x, " ", show y]
 ,   testCase "zeroPos is the origin" $ assertEqual "" L.zeroPos (L.IntPos 0 0)
 ,   testProperty "x extracts the first coordinate" $ 
        \(x :: Int) -> 
        forAll (arbitrary `suchThat` \v -> v /= x) $ \(y :: Int) ->
        L.x (L.IntPos x y) === x
 ,   testProperty "y extracts the second coordinate" $ 
        \(x :: Int) -> 
        forAll (arbitrary `suchThat` \v -> v /= x) $ \(y :: Int) ->
        L.y (L.IntPos x y) === y
 ,   testEqLaws (Proxy @(L.Pos Int))
 ,   testProperty "Show instance for Pos works" $ 
        \(x :: Int) -> \(y :: Int) ->
        show (L.Pos x y) === concat ["Pos ", show x, " ", show y]
 ,   testCase "zero is the origin" $ assertEqual "" L.zero (L.Pos 0 0)
 ,   testProperty "left extracts the first coordinate" $ 
        \(x :: Int) -> 
        forAll (arbitrary `suchThat` \v -> v /= x) $ \(y :: Int) ->
        L.left (L.Pos x y) === x
 ,   testProperty "top extracts the second coordinate" $ 
        \(x :: Int) -> 
        forAll (arbitrary `suchThat` \v -> v /= x) $ \(y :: Int) ->
        L.top (L.Pos x y) === y
 ,   testCase "render produces the expected results" $ do
        assertEqual "" "" (L.render [] 1)
        assertEqual "" "I am item #1\nThere. Are. 2. Items.\n" (L.render (tail L.doc) 1)
        assertEqual "" "1. An item\nI am item #2\nThere. Are. 3. Items.\n" (L.render L.doc 1)
 ]

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
