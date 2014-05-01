module ZuriRBM.MNIST where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (getWord32be, isEmpty, runGet)
import Data.Binary
import Control.Monad (replicateM, liftM)

import ZuriRBM.TrainingData

-- | Pure Random Number Generator with Random Monad implementation
--import System.Random.Mersenne.Pure64
--import Control.Monad.Mersenne.Random

data MNIST = MNIST {
    mnistTrainingCases :: [(Image, Label)],
    mnistTestCases     :: [(Image, Label)] 
}

instance Show MNIST where
  show (MNIST train test)  = "<MNIST 28 x 28, " ++ tr ++  ", " ++ te ++ ">"
    where tr = show $ length train
          te = show $ length test
 

mnistFromImagesAndLabels :: (ImageSet, LabelSet) -> (ImageSet, LabelSet) -> MNIST
mnistFromImagesAndLabels (trainis, trainls) (testis, testls) = MNIST trainingcases testcases 
    where trainingcases = zip trainingimages traininglabels
          testcases = zip testimages testlabels
          trainingimages = imageSetImages trainis
          traininglabels = labelSetLabels trainls
          testimages = imageSetImages testis
          testlabels = labelSetLabels testls

readMNISTFromFilePath :: String -> IO MNIST
readMNISTFromFilePath p = do
    trainingimages <- BL.readFile $ p ++ "/train-images-idx3-ubyte"
    traininglabels <- BL.readFile $ p ++ "/train-labels-idx1-ubyte"
    testimages     <- BL.readFile $ p ++ "/t10k-images-idx3-ubyte"
    testlabels     <- BL.readFile $ p ++ "/t10k-labels-idx1-ubyte"

    let trainis = runGet readImageSet trainingimages
        trainls = runGet readLabelSet traininglabels
        testis  = runGet readImageSet testimages
        testls  = runGet readLabelSet testlabels
        mnist = mnistFromImagesAndLabels (trainis, trainls) (testis, testls)

    return mnist


 {-
instance HasTestcases TestFactory where
    readFromFilePath      = const . TestFactory $ pureMT 45
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
-}

data LabelSet = LabelSet {
   labelSetMagicNumber    :: Int,
   labelSetNumberOfLabels :: Int,
   labelSetLabels         :: [String]
}

instance Show LabelSet where
  show (LabelSet _ n _) = "<LabelSet " ++ show n ++ ">"

type Image = [Word8]

instance Show ImageSet where
   show (ImageSet _ nr r c _) = "<ImageSet " ++ show (nr, r, c) ++ ">"

data ImageSet = ImageSet {
   imageSetMagicNumber     :: Int,
   imageSetNumberOfImages  :: Int,
   imageSetNumberOfRows    :: Int,
   imageSetNumberOfColumns :: Int,
   imageSetImages :: [Image]
}

readLabelSet :: Get LabelSet
readLabelSet = do
      magicNumber <- getWord32be
      numberOfLabels <- getWord32be
      labels <- readLabels (fromIntegral numberOfLabels)
      return $ LabelSet (fromIntegral magicNumber) (fromIntegral numberOfLabels) labels

readLabels :: Int -> Get [String]
readLabels n = replicateM n readLabel 

readLabel :: Get String
readLabel = liftM (show) getWord8

readImageSet :: Get ImageSet
readImageSet = do
    magicNumber <- getWord32be
    numberOfImages <- getWord32be
    r <- getWord32be
    c <- getWord32be

    let rc = fromIntegral r
    let cc = fromIntegral c
    let nrimgs = fromIntegral numberOfImages

    images <- (readImages' nrimgs rc cc)

    return $ ImageSet 
               (fromIntegral magicNumber)
               nrimgs
               rc 
               cc 
               images 

readImages' :: Int -> Int -> Int -> Get [Image]
readImages' n w h = replicateM n $ readImage w h

readImage :: Int -> Int -> Get Image
readImage w h = replicateM (w * h) getWord8


