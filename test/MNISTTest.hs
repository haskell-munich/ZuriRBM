module Main where

import ZuriRBM.TrainingData
import ZuriRBM.MNIST

main = do mnist <- readMNISTFromFilePath "/Users/halcyon/Documents/Coding/ZuriRBM/data"
          putStrLn $ show mnist
