module Main where

import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.State
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import ZuriRBM.TrainingData
import ZuriRBM.Learning
import ZuriRBM.MNIST

main = putStrLn "Hello RBM!"

unsafeGetMNIST = unsafePerformIO $ readFromFilePath "../data" :: MNIST