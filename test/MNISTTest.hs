module Main where

import ZuriRBM.TrainingData
import ZuriRBM.MNIST

main = do mnist <- readFromFilePath "./data" :: IO MNIST
          putStrLn $ show mnist
