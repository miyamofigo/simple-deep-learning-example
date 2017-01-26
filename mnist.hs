import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.HashMap as HashMap
import qualified Numeric.LinearAlgebra.Data as HMatrix

import Data.Char
import Data.Either.Unwrap
import Data.HashMap 
import Network.Download 
import Numeric.LinearAlgebra.Data 
import System.Directory
import System.FilePath
import System.IO

urlBase = "http://yann.lecun.com/exdb/mnist/"
keyFile = HashMap.fromList 
  [ ("train_img"  , "train-images-idx3-ubyte.gz")
  , ("train_label", "train-labels-idx1-ubyte.gz")
  , ("test_img"   , "t10k-images-idx3-ubyte.gz")
  , ("test_label" , "t10k-labels-idx1-ubyte.gz") ]

saveFile = toFilePath "mnist.pkl"

trainNum  = 60000
testNum   = 10000
imageDim  = (1, 28, 28)
imageSize = 784

download fileName = do
    filePath <- toFilePath fileName
    isExists <- doesFileExist filePath
    if isExists 
        then return () 
        else do
            res <- openURI $ urlBase ++ fileName 
            putStrLn $ "Downloading " ++ fileName ++ " ... "
            withFile filePath 
                WriteMode 
                (\handle -> do
                     hPrint handle $ fromRight res) 
            putStrLn "Done."

downloadMinst = mapM_ download $
    elems keyFile 

toFilePath fileName = do
    datasetDir <- getCurrentDirectory
    return $ datasetDir 
        ++ "/" ++ fileName 

charsToDoubles::[Char] -> [R]
charsToDoubles = fmap (fromIntegral . digitToInt)

doLoadLabel fileName = do
    content <- doLoadFromGZip fileName
    return $ HMatrix.fromList $ 
        charsToDoubles $ drop 8 content 

doLoadImage fileName = do
    content <- doLoadFromGZip fileName
    lst <- return $ 
        charsToDoubles $ drop 16 content 
    putStrLn "Done."
    return $ HMatrix.fromList lst 

doLoadFromGZip fileName = do
    filePath <- toFilePath fileName
    putStrLn $ "Converting " 
        ++ fileName ++ "to utf8 array ..."
    compressed <- withFile filePath
        ReadMode
        (\handle -> hGetContents handle)
    content <- fmap GZip.decompress $ return $
        Char8.pack compressed
    putStrLn "Done" 
    return $ Char8.unpack content

main = downloadMinst  
