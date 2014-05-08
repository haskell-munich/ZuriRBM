module ZuriRBM.Learning where

import Numeric.LinearAlgebra


contrastiveDivergence = undefined



cdStep :: WeightMatrix -> StateVector -> StateVector -> ?

type StateVector = Vector Double
type CoactivityMatrix = Matrix Double

coactivityMatrix :: StateVector -> StateVector -> CoactivityMatrix
coactivityMatrix a b = outer a b