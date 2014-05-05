module ZuriRBM.Matrix where

import Data.List (intercalate)
import System.IO
import qualified Data.ByteString as BS
import Data.Word (Word8(..))

class Matrix a where
    cons :: (Int, Int) -> a
    writeToPBM :: String -> a -> IO ()

data ListMatrix = ListMatrix {
    matrixDimension :: (Int, Int),
    matrixData :: [Double]
}

instance Show ListMatrix where
    show (ListMatrix (n, m) l) = matrixPrint ", " "\n" n m l

matrixPrint :: (Show a) => String -> String -> Int -> Int -> [a] -> String
matrixPrint sep cr 0 _ l = commaSeparatedShow sep l 
matrixPrint sep cr n m l = commaSeparatedShow sep (take m l) ++ cr ++ (matrixPrint sep cr (n - 1) m (drop m l))
commaSeparatedShow sep l = intercalate sep $ foldr ((:) . show) []  l

instance Matrix ListMatrix where
    cons dim@(n, m) = ListMatrix dim $ take (n * m) $ repeat 1.0 
    writeToPBM = listMatrixToPBM True

listMatrixToPGM :: Bool -> String -> ListMatrix -> IO ()
listMatrixToPGM binary path mat = do 
        let magicnumber = if binary then "P5\n" else "P2\n"
            (n, m) = matrixDimension mat
            integermatrixdata = map (floor . (*255.0)) $ matrixData mat :: [Word8]
        f <- openFile path WriteMode
        hPutStr f magicnumber
        hPutStr f $ show n ++ " " ++ show m ++ "\n" -- width x height
        hPutStr f $ "255\n" -- max grayscale value
        if binary then do
            hSetBinaryMode f True
            BS.hPut f . BS.pack $ integermatrixdata
        else do
            hPutStr f $ matrixPrint " " "\n" n m integermatrixdata 
        hClose f
