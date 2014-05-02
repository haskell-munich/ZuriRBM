module Main where

import ZuriRBM.TrainingData
import ZuriRBM.MNIST

main = do mnist <- readFromFilePath "/Users/halcyon/Documents/Coding/ZuriRBM/data" :: IO MNIST
          putStrLn $ show mnist
