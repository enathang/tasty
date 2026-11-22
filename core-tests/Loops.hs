{-# OPTIONS_GHC -fno-full-laziness #-}

module Loops (testLoops) where

import Control.Concurrent (MVar, newEmptyMVar, rtsSupportsBoundThreads, takeMVar)
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Utils

loopTree :: TestTree
loopTree = testGroup "loops"
  [ testCase "self-referential thunk" $ do
      let x :: Int
          x = x + 1
      _ <- evaluate x
      return ()
  , testCase "MVar nobody fills" $ do
      m <- newEmptyMVar :: IO (MVar ())
      takeMVar m
  ]

threw :: Exception e => (e -> Bool) -> Result -> Bool
threw p r = case resultOutcome r of
  Failure (TestThrewException e) -> maybe False p (fromException e)
  _ -> False

testLoops :: TestTree
testLoops = testGroup "Run if threading is enabled" $
  if rtsSupportsBoundThreads then [testLoopDetection] else []

testLoopDetection :: TestTree
testLoopDetection = testCase "Loop and deadlock detection" $ do
  res <- launchTestTree mempty (localOption (mkTimeout (5*10^(6 :: Int))) loopTree) $ \smap -> do
    rs <- runSMap smap
    return $ const $ return rs
  case res of
    [loopR, mvarR] -> do
      assertBool ("expected <<loop>>, got: " ++ resultDescription loopR)
        (threw (\NonTermination -> True) loopR)
      assertBool ("expected BlockedIndefinitelyOnMVar, got: " ++ resultDescription mvarR)
        (threw (\BlockedIndefinitelyOnMVar -> True) mvarR)
    rs -> assertFailure ("expected 2 results, got " ++ show (length rs))
