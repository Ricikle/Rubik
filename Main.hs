module Main where
  import System.Directory
  import Cube
  import qualified Stage1 as Stage1
  import qualified Stage2 as Stage2
  import qualified Stage3 as Stage3
  import qualified Stage4 as Stage4
  import qualified CubeReader as Reader


  checkFile :: FilePath -> IO Bool
  checkFile fp = do
    b <- doesFileExist fp
    if b then do
      putStrLn $ "Found " ++ fp
      r <- readFile fp
      if not . null $ r then
        return True
        else (putStrLn $ fp ++ " is empty") >> return False
      else putStrLn (fp ++ " not found") >> return False

  main :: IO ()
  main = do
    s1 <- checkFile "Stage1.dat"
    if s1 then return () else putStrLn "Writing Table for Stage 1" >> Stage1.writeTable
    s2 <- checkFile "Stage2.dat"
    if s2 then return () else putStrLn "Writing Table for Stage 2" >> Stage2.writeTable
    s3 <- checkFile "Stage3.dat"
    if s3 then return () else putStrLn "Writing Table for Stage 3" >> Stage3.writeTable
    s4 <- checkFile "Stage4.dat"
    if s4 then return () else putStrLn "Writing Table for Stage 4" >> Stage4.writeTable
    cube <- Reader.readCube
    putStrLn $ show cube
