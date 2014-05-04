module Matrix where

import Data.List (intercalate)
import System.IO
import qualified Data.ByteString.Char8 as BS

class Matrix a where
    cons :: (Int, Int) -> a
    writeToPBM :: String -> a -> IO ()

data ListMatrix = ListMatrix {
    matrixDimension :: (Int, Int),
    matrixData :: [Double]
}

instance Show ListMatrix where
    show (ListMatrix (n, m) l) = matrixPrint n m l "\n"

matrixPrint 0 _ l s = commaSeparatedShow l 
matrixPrint n m l s = commaSeparatedShow (take m l) ++ s ++ (matrixPrint (n - 1) m (drop m l) s)
commaSeparatedShow l = intercalate ", " $ foldr ((:) . show) []  l

instance Matrix ListMatrix where
    cons (n, m) = ListMatrix (n, m) $ take (n * m) $ repeat 1.0 
    writeToPBM = listMatrixToPBM

listMatrixToPBM :: String -> ListMatrix -> IO ()
listMatrixToPBM path m = do 
        f <- openFile path WriteMode  
        hPutStr f "P5\n" -- Magic number
        let (w, h) = matrixDimension m
        hPutStr f $ show h ++ " " ++ show w ++ "\n" -- width x height
        hPutStr f $ "255\n" -- max grayscale value
        hSetBinaryMode f True
        BS.hPut f $ foldr (BS.append . BS.pack . show . floor . (*255)) BS.empty $ matrixData m 

 

