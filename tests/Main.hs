{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import Data.AdaBoost
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testProperty "test against a reference test case"
                 testAgainstReferenceCase
    ]

testAgainstReferenceCase :: Bool
testAgainstReferenceCase =
    let classifier3 = classifiers !! 2
     in flip all training_data $ \(inp, correct_class) ->
            correct_class == classifier3 inp
  where
    classifiers = simpleAdaBoost training_data
                   (\(_ :: Weights Double) -> flip concatMap [-0.5,0.5..9.5] $ \x ->
                       [\dbl -> if dbl < x then class1 else class2])

    training_data :: [(Double, Class)]
    training_data =
        [(0, class1)
        ,(1, class1)
        ,(2, class1)
        ,(3, class2)
        ,(4, class2)
        ,(5, class2)
        ,(6, class1)
        ,(7, class1)
        ,(8, class1)
        ,(9, class2)]

