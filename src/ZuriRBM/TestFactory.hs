module ZuriRBM.TestFactory where

import ZuriRBM.TrainingData

-- | Pure Random Number Generator with Random Monad implementation
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random

import Control.Monad (replicateM)

data TestFactory = TestFactory PureMT deriving Show

instance HasTestcases TestFactory where
    readFromFilePath      = return $ const . TestFactory $ pureMT 45
    numberOfTestCases     = const 0
    numberOfTrainingCases = const 100
    sampleNewTestCase     = undefined
    sampleNewTrainingCase = testFactorySampleNewTrainingCase


testFactorySampleNewTrainingCase (TestFactory rng) =
    let (boolvector, rng') = runRandom (sampleBooleanVector 10) rng
        testcase = (boolvector, Nothing)
        s' = TestFactory rng'
    in  (testcase, s')

sampleBooleanVector :: Int -> Rand [Bool]
sampleBooleanVector n = replicateM n getBool 