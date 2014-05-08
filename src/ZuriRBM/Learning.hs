module ZuriRBM.Learning where

import Numeric.LinearAlgebra(Matrix(..), outer)
import System.Random.Mersenne.Pure64(PureMT)
import ZuriRBM.TrainingData(StateVector, VisibleUnitVector, HiddenUnitVector)

type CoactivityMatrix = Matrix Double
type WeightMatrix     = Matrix Double
data RBMState = RBMState
    { rbmRNGState :: PureMT
    , rbmWeightmatrix :: WeightMatrix
    , rbmHiddenUnits :: HiddenUnitVector
    , rbmVisibleUnits ::VisibleUnitVector
	}

contrastiveDivergence = undefined

cdStep :: RBMState -> RBMState
cdStep = undefined

coactivityMatrix :: StateVector -> StateVector -> CoactivityMatrix
coactivityMatrix = outer