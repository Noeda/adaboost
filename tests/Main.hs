{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main ( main ) where

import Data.AdaBoost
import Data.Functor.Identity
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testProperty "test against a reference test case"
                   testAgainstReferenceCase
    , testProperty "test against a reference test case (monadic)"
                   testAgainstReferenceCase2
    ]

testAgainstReferenceCase :: Bool
testAgainstReferenceCase =
    let classifier3 = classifiers !! 2
     in flip all trainingData $ \(inp, correct_class) ->
            correct_class == classifier3 inp
  where
    classifiers = simpleAdaBoost trainingData
                   (\(_ :: Weights Double) -> flip fmap [-0.5,0.5..9.5] $
                     \x -> \dbl -> if dbl < x then class1 else class2)

newtype Comp = Comp Double
               deriving ( Show )

instance Monad m => ClassifierLike Comp m Double where
    toClassifierFunction (Comp x) i = return $ if x < i then class1 else class2

testAgainstReferenceCase2 :: Bool
testAgainstReferenceCase2 = runIdentity $ do
    producer <- fmap unwrapClassifierProducer $
                simpleAdaBoostM trainingData
                (\(_ :: Weights Double) -> return $ fmap Comp [-0.5,0.5..9.5])
    producer2 <- fmap unwrapClassifierProducer $ snd producer
    producer3 <- fmap unwrapClassifierProducer $ snd producer2
    return $ flip all trainingData $ \(inp, correct_class) ->
        let r = runIdentity (toClassifierFunction (fst producer3) inp)
         in correct_class == r

trainingData :: [(Double, Class)]
trainingData =
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

