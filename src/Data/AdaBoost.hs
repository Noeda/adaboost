-- | This module implements functions that can AdaBoost your classifiers.
--
-- Some effort has been made to be generic enough that the following features
-- should be within reach for the user:
--
--     * Training data does not need to be loaded in memory. This should allow very large training sets.
--
--     * Weak learners can be trained in parallel.
--
--     * You can have control over how many iterations you want to run.
--
--     * The combined, AdaBoosted classifier can be serialized, if passed
--       serializable weak learners. With this you can load pre-trained
--       AdaBoosted classifiers.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Data.AdaBoost
    (
      adaBoost
    , simpleAdaBoost
    , simpleAdaBoostM
    , BoostedClassifier()
    , baseClassifiers
    -- * Classes
    , Class()
    , class1
    , class2
    -- * Type synonyms
    , NumberOfTrainingItems
    , NumberOfClassifiers
    , NthTrainingItem
    , Weights
    , NthClassifier
    , Classifier
    , ClassifierLike(..)
    , ClassifierProducer(..)
    , Combiner
    -- * Combining strategies
    , forkIOIndexedM
    , forIndexedM )
    where

import Control.Concurrent.Async
import Control.Monad hiding ( forM_ )
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Data
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VG
import GHC.Generics

-- | How many training items are there?
type NumberOfTrainingItems = Int

-- | How many classifiers are there?
type NumberOfClassifiers = Int

-- | Type of an index, to Nth training item.
type NthTrainingItem = Int

-- | Type of a vector of weights, accessed by a function.
type Weights f = Int -> f

-- | Type of an index, to Nth classifier.
type NthClassifier = Int

type Classifier m input = input -> m Class

newtype Class = Class Bool
                deriving ( Eq, Enum, Ord, Show, Read, Typeable, Data, Generic )

-- | Class of things that can be turned into plain `Classifier`s.
--
-- This type class exists to make it possible to use serializable classifiers
-- in the algorithm. Haskell functions cannot be serialized right now.
class ClassifierLike a m input | a -> input where
    toClassifierFunction :: a -> Classifier m input

instance ClassifierLike (Classifier m input) m input where
    toClassifierFunction = id
    {-# INLINE toClassifierFunction #-}

newtype BoostedClassifier (m :: * -> *) classifier weight input = BoostedClassifier
        { baseClassifiers :: [(weight, classifier)] }
        deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance (Functor m, Monad m, RealFloat weight, ClassifierLike classifier m input)
      => ClassifierLike (BoostedClassifier m classifier weight input)
                        m input where
    toClassifierFunction (BoostedClassifier multipliers) inp = do
        result <- fmap getSum $ execWriterT $ forM_ multipliers $ \(at, ht) -> do
            result <- lift $ toClassifierFunction ht inp
            tell $ Sum $ at * if result == class1
                then -1
                else 1
        return $ if result <= 0 then class1 else class2
    {-# INLINE toClassifierFunction #-}

class1 :: Class
class1 = Class True

class2 :: Class
class2 = Class False

-- | Train an AdaBoosted classifier.
--
-- This is the most generic function in this module and may not be the most
-- convenient to use as-is.
--
-- The asymptotic memory requirements of this function are likely dictated by the need to
-- store weights which will be @ sizeOf weight * NumberOfTrainingItems @ or
-- number of classifiers per iteration (if you have many of them).
--
-- In particular, the contents of the training data itself are not demanded to
-- be in memory all the time so your individual training items can be
-- super-large if you want.
--
-- Memory requirements: O(max(N, M, P)) where N is the number of training items
-- , M is the number of classifiers in some iteration and P is the number of
-- iterations.
--
adaBoost :: forall m classifier input weight.
            (Functor m, Monad m, RealFloat weight, V.Unbox weight
            ,ClassifierLike classifier m input)
         => (NthTrainingItem -> m (input, Class))
         -- ^ Access Nth training data item. It can be undefined for any N that
         -- is equal or larger to given `NumberOfTrainingItems` and for
         -- negative values.
         -> NumberOfTrainingItems
         -- ^ How many training items are there?
         -> (Weights weight -> m (NumberOfClassifiers, NthClassifier -> m classifier))
         -- ^ Given weights, return an action that returns a set of
         --   classifiers, accessed by an indexing function. Often the weights
         --   are not used but you can use them if you want to be informed
         --   which training items are being problematic.
         -> (forall b c. Combiner m b c)
         -- ^ A combiner function.
         -> m (ClassifierProducer m classifier weight input) -- ^ Produces classifiers.
adaBoost training_data training_data_size classifier_trainer combiner =
    iteration initial_weights []
  where
    initial_weights :: V.Vector weight
    initial_weights = V.replicate training_data_size (1/fromIntegral training_data_size)

    iteration :: V.Vector weight
              -> [(weight, classifier)]
              -> m (ClassifierProducer m classifier weight input)
    iteration weights multipliers = do
        (num_classifiers, classifier) <- classifier_trainer iweights

        -- find the best suited classifier for this iteration
        (chosen_classifier, cerror, incorrects) <- combiner
                 num_classifiers
                 (error "adaBoost: must have at least one classifier.")
                 classifier
                 (\f@(_, cerror1, _) s@(_, cerror2, _) ->
                     return $ if (abs $ 0.5-cerror1) > (abs $ 0.5-cerror2)
                       then f
                       else s)
                 (\c -> do
                     (x, y) <- measure_errors weights (toClassifierFunction c)
                     return (c, x, y))

        let new_multipliers = (activate cerror, chosen_classifier):multipliers
            next_weights = compute_next_weights weights cerror incorrects

        return $ ClassifierProducer ( final_classifier new_multipliers
                                    , iteration next_weights new_multipliers )
      where
        iweights = (weights V.!)

    activate classifier_score = log ((1-classifier_score)/classifier_score)

    final_classifier multipliers = BoostedClassifier multipliers

    compute_next_weights :: V.Vector weight
                         -> weight
                         -> IS.IntSet
                         -> V.Vector weight
    compute_next_weights old_weights classifier_score incorrects =
        normalize $ flip V.imap old_weights $ \idx old_weight ->
            old_weight * if IS.member idx incorrects
                then exp activated
                else 1
      where
        activated = activate classifier_score

    measure_errors :: V.Vector weight
                   -> Classifier m input
                   -> m (weight, IS.IntSet)
    measure_errors weights classifier = flip runStateT IS.empty $ fmap getSum $
        execWriterT $ for_ [0..training_data_size-1] $ \idx -> do
            (input, real_answer) <- lift $ lift $ training_data idx
            proposed_answer <- lift $ lift $ classifier input
            when (proposed_answer /= real_answer) $ do
                tell $ Sum $ weights V.! idx
                lift $ modify (IS.insert idx)
{-# INLINE adaBoost #-}

-- | Similar to `simpleAdaBoost` but allows you to use a monad and a
-- `ClassifierLike` instead of a simple function.
simpleAdaBoostM :: forall m f f2 weight classifier input.
                   (Functor m, Monad m, Foldable f, Foldable f2, RealFloat weight, V.Unbox weight, ClassifierLike classifier m input)
             => f (input, Class)
             -- ^ Training data.
             -> (Weights weight -> m (f2 classifier))
             -- ^ Classifier generator.
             -> m (ClassifierProducer m classifier weight input)
simpleAdaBoostM training_data' generator = do
    adaBoost (\i -> return $ training_data VG.! i)
             (VG.length training_data)
             (\wgts -> monadic_generator wgts)
             forIndexedM
  where
    training_data = VG.fromList $ toList training_data'

    monadic_generator :: Weights weight
                      -> m (NumberOfClassifiers
                           , NthClassifier -> m classifier)
    monadic_generator weights = do
        folding <- generator weights
        let classifiers = VG.fromList $ toList folding
        return ( VG.length classifiers
               , \i -> return $ classifiers VG.! i )
{-# INLINE simpleAdaBoostM #-}

-- | Go through a structure that can be indexed, calculate some result for each
-- value and then combine all the results.
type Combiner m a b = Int
                   -> b
                   -> (Int -> m a)
                   -> (b -> b -> m b)
                   -> (a -> m b)
                   -> m b

newtype ClassifierProducer m classifier weight input = ClassifierProducer
    { unwrapClassifierProducer ::
      ( BoostedClassifier m classifier weight input
      , m (ClassifierProducer m classifier weight input)) }

-- | Simple indexer.
forIndexedM :: Monad m => Combiner m a b
forIndexedM num_items zero indexer combiner folding
    | num_items == 0 = return zero
    | otherwise = do
        initial <- indexer 0 >>= folding
        recursively 1 initial
  where
    recursively x !accum
        | x >= num_items = return accum
        | otherwise =
            indexer x >>= folding >>= combiner accum >>= recursively (x+1)

-- | `forkIO` based indexer. Each value is calculated under `forkIO` and then
-- combined in a tree.
forkIOIndexedM :: Combiner IO a b
forkIOIndexedM num_items zero indexer combiner folding
    | num_items == 3 =
        withAsync (indexing_at 0) $ \i1 ->
        withAsync (indexing_at 1) $ \i2 -> do
            last_v <- indexing_at 2
            i1_v <- wait i1
            i2_v <- wait i2
            c1 <- combiner i1_v i2_v
            combiner c1 last_v
    | num_items == 2 =
        withAsync (indexing_at 0) $ \i1 ->
        withAsync (indexing_at 1) $ \i2 -> do
            i1_v <- wait i1
            i2_v <- wait i2
            combiner i1_v i2_v
    | num_items == 1 = indexing_at 0
    | num_items <= 0 = return zero

    -- num_items >= 4
    | otherwise =
        withAsync (forkIOIndexedM (num_items `div` 2)
                                  zero
                                  indexer
                                  combiner
                                  folding) $ \c1 ->
        withAsync (forkIOIndexedM ((num_items+1) `div` 2)
                                  zero
                                  (\x -> indexer (x+(num_items `div` 2)))
                                  combiner
                                  folding) $ \c2 -> do
            i1_v <- wait c1
            i2_v <- wait c2
            combiner i1_v i2_v
  where
    indexing_at n = indexer n >>= folding
{-# INLINE forkIOIndexedM #-}

-- | Simplistic AdaBoost.
--
-- This one is designed to take in \"easy\" data such as lists, making it more
-- practical to use in quick throw-away programs or maybe even ghci.
--
-- Returns an infinite list of classifiers, where @ lst !! n @ returns the
-- AdaBoosted classifier after N iterations.
simpleAdaBoost :: (Foldable f, Foldable f2, RealFloat weight, V.Unbox weight)
               => f (input, Class)
               -- ^ Training data.
               -> (Weights weight -> f2 (input -> Class))
               -- ^ Classifier generator.
               -> [input -> Class]
               -- ^ Infinite list of classifiers.
simpleAdaBoost training_data' generator = runIdentity $ do
    producer <- adaBoost (\i -> Identity $ training_data VG.! i)
                         (VG.length training_data)
                         (Identity . monadic_generator)
                         forIndexedM
    Identity $ listify producer
  where
    training_data = VG.fromList $ toList training_data'

    listify producer =
        let (classifier, next) = unwrapClassifierProducer producer
         in (\inp -> runIdentity $ toClassifierFunction classifier inp):
            listify (runIdentity next)

    monadic_generator weights =
        (VG.length classifiers, \i -> Identity $ \inp ->
                                      Identity $ (classifiers VG.! i) inp)
      where
        classifiers = VG.fromList $ toList folding
        folding = generator weights
{-# INLINE simpleAdaBoost #-}

normalize :: (RealFloat a, V.Unbox a) => V.Vector a -> V.Vector a
normalize vec = let total = V.sum vec
                 in V.map (/ total) vec

