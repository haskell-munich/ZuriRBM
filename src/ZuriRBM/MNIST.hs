module ZuriRBM.MNIST (MNIST(..)) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (getWord32be, isEmpty, runGet)
import Data.Binary
import Control.Monad (replicateM, liftM)

import ZuriRBM.TrainingData

-- | Pure Random Number Generator with Random Monad implementation
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random

data MNIST = MNIST {
    mnistTrainingCases :: [(Image, Label)],
    mnistTestCases     :: [(Image, Label)],
    mnistRNG           :: PureMT,
    mnistTrainingCaseIndex :: Int,
    mnistTestCaseIndex     :: Int
}

instance Show MNIST where
  show (MNIST train test _ tri tei)  = "<MNIST 28 x 28, " ++ tr ++  ", " ++ te ++  ", " ++ show tri ++  ", "  ++ show tei ++ ">"
    where tr = show $ length train
          te = show $ length test
 

mnistFromImagesAndLabels :: (ImageSet, LabelSet) -> (ImageSet, LabelSet) -> MNIST
mnistFromImagesAndLabels (trainis, trainls) (testis, testls) = MNIST trainingcases testcases rng 0 0
    where trainingcases = zip trainingimages traininglabels
          testcases = zip testimages testlabels
          trainingimages = imageSetImages trainis
          traininglabels = labelSetLabels trainls
          testimages = imageSetImages testis
          testlabels = labelSetLabels testls
          rng = pureMT 42

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

instance HasTestcases MNIST where
    readFromFilePath      = readMNISTFromFilePath
    numberOfTestCases     (MNIST _  testcs _ _ _) = length testcs
    numberOfTrainingCases (MNIST traincs _ _ _ _) = length traincs
    sampleNewTestCase     = sampleBoolifiedTestCase
    sampleNewTrainingCase = sampleBoolifiedTrainingCase

sampleBoolifiedTestCase :: MNIST -> (TrainingCase, MNIST)
sampleBoolifiedTestCase (MNIST trcs tecs rng trix teix) =
    let (testcase, label) = tecs!!teix
        (boolvector, rng') = runRandom (mapM sampleWithThreshold testcase) rng
        sampledcase = (boolvector, Just label)
        s' = MNIST trcs tecs rng' trix (teix + 1)
    in  (sampledcase, s')

sampleBoolifiedTrainingCase :: MNIST -> (TrainingCase, MNIST)
sampleBoolifiedTrainingCase (MNIST trcs tecs rng trix teix) =
    let (testcase, label) = trcs!!trix
        (boolvector, rng') = runRandom (mapM sampleWithThreshold testcase) rng
        sampledcase = (boolvector, Just label)
        s' = MNIST trcs tecs rng' (trix + 1) teix
    in  (sampledcase, s')


sampleWithThreshold :: Word8 -> Rand Bool
sampleWithThreshold th = do
  r64 <- getWord64
  let r8 = fromIntegral $ r64 `mod` 256
      ordering = compare r8 th
  case ordering of
          LT -> getBool >> return True
          GT -> getBool >> return False
          EQ -> getBool


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


