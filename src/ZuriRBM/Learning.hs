module ZuriRBM.Learning where

import Data.Packed.Vector(Vector(..), fromList)
import Numeric.LinearAlgebra(
		Matrix(..), step, outer, ident, trans, mapVector, zipVectorWith,
		(><), (|>), (<>), cols
	)
import System.Random.Mersenne.Pure64(PureMT, pureMT)
import Control.Monad.Mersenne.Random
import Control.Monad.State
import Control.Monad(replicateM)
import ZuriRBM.TrainingData(StateVector, VisibleUnitVector, HiddenUnitVector)

type CoactivityMatrix = Matrix Double
type WeightMatrix     = Matrix Double
data RBMState = RBMState
    { rbmRNGState :: PureMT
    , rbmWeightmatrix :: WeightMatrix
    , rbmVisibleUnits ::VisibleUnitVector
    , rbmHiddenUnits :: HiddenUnitVector
	} deriving Show

contrastiveDivergence = undefined

dummyRBMState = RBMState
	(pureMT 42)
	((3><4) [0,0.2,0.9,-0.2, 0.1,-0.3,-0.8,-1.1, -0.2,0.3,0.5,0.4])
	(3 |> [1,0,1])
	(4 |> [1,0,0,0])

cdStep :: State RBMState ()
cdStep = undefined

sampleHiddenFromVisible :: State RBMState ()
sampleHiddenFromVisible = do
	w <- getWeightMatrix
	v <- getVisibleUnits
	let nrofhiddenunits = cols w
	dicerolllist <-  runRBMRand (sampleDoubles nrofhiddenunits)
	let probabilities = mapVector sigmoid $ (trans w) <> v
	    dicerolls = fromList $ dicerolllist :: Vector Double
	    h = step $ zipVectorWith subtract probabilities dicerolls
	putHiddenUnits h

sigmoid :: Double -> Double
sigmoid x = 1/(1+(exp (negate x)))

sampleDoubles :: Int -> Rand [Double]
sampleDoubles n = replicateM n getDouble

runRBMRand :: Rand a -> State RBMState a
runRBMRand action = do
	rng <- getRBMRNG
	let (result, rng') = runRandom action rng
	putRBMRNG rng'
	return result

getWeightMatrix :: State RBMState WeightMatrix
getWeightMatrix = do
	rbm <- get
	return $ rbmWeightmatrix rbm

getVisibleUnits :: State RBMState VisibleUnitVector
getVisibleUnits = do
	rbm <- get
	return $ rbmVisibleUnits rbm

getRBMRNG :: State RBMState PureMT
getRBMRNG = do
	rbm <- get
	return $ rbmRNGState rbm

putRBMRNG :: PureMT -> State RBMState ()
putRBMRNG rng = do
	(RBMState _ w v h) <- get
	put $ RBMState rng w v h

putHiddenUnits :: HiddenUnitVector -> State RBMState ()
putHiddenUnits h = do
	(RBMState rng w v _) <- get
	put $ RBMState rng w v h


coactivityMatrix :: StateVector -> StateVector -> CoactivityMatrix
coactivityMatrix = outer