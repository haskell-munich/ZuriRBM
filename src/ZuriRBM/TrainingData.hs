module ZuriRBM.TrainingData (
		VisibleUnitVector, TrainingCase, Label,
		HasTestcases(readFromFilePath, numberOfTestCases, numberOfTrainingCases, sampleNewTestCase, sampleNewTrainingCase)
	) where

type Label = String
-- | Boolean Vector as input for the visible units
type VisibleUnitVector = [Bool]
type TrainingCase = (VisibleUnitVector, Maybe Label)

-- | typeclass defining the functionality of a TestCase Factory!
class HasTestcases s where
    readFromFilePath      :: String -> IO s         -- ^ read factory state from a file
    numberOfTestCases     :: s -> Integer           -- ^ magnitude of available test cases
    numberOfTrainingCases :: s -> Integer           -- ^ magnitude of available training cases
    sampleNewTestCase     :: s -> (TrainingCase, s)
    sampleNewTrainingCase :: s -> (TrainingCase, s) -- ^ sample a new test case (optionally stochastically)
