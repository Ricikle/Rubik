{-# LANGUAGE ViewPatterns #-}

module Stage4 (writeTable,getMoveList) where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.Trans.State
  import Control.Monad
  import Control.Monad.IO.Class
  import Data.Bits
  import Data.Array
  import Data.Word

  type BitArr = Word64

  movesStage4 :: [Move]
  movesStage4 = [U2, L2, R2, D2, B2, F2]

  adaptedMoves :: Array Int Cube
  adaptedMoves = listArray (1,6) $ map (\f -> f identity) [u2,l2,r2,d2,b2,f2]

  getMoveList :: Cube -> IO [Move]
  getMoveList c = readTable "Stage4.dat" >>= \t ->
    return $ getMoveListStage4 t (toBitArr c)

  getMoveListStage4 :: Table BitArr -> BitArr -> [Move]
  getMoveListStage4 ma b = if b == toBitArr identity
    then [] else let m = ma Map.! b
                 in m : getMoveListStage4 ma (applyBitArr b m)

  toBitArr :: Cube -> BitArr
  toBitArr c = toBits $ map f (toList (edges c)) ++ map g (toList (corner c)) where
    toBits :: [Int] -> BitArr
    toBits [] = 0
    toBits (x:xs) = fromIntegral x + shift (toBits xs) 2
    f 1 = 0
    f 2 = 0
    f 3 = 1
    f 4 = 1
    f 5 = 2
    f 6 = 2
    f 7 = 3
    f 8 = 3
    f x = x - 9
    g x = (x-1) `div` 2


  applyBitArr :: BitArr -> Move -> BitArr
  applyBitArr b m = let i = case m of
                            U2 -> 1
                            L2 -> 2
                            R2 -> 3
                            D2 -> 4
                            B2 -> 5
                            F2 -> 6
                      in sum ([shift ((if testBit b (2*k-1) then 2 else 0) + (if testBit b (2*k-2) then 1 else 0)) (2*((k !> edges (adaptedMoves ! i)) - 1))| k <- [1..12]]
                        ++ [shift ((if testBit b (2*k+23) then 2 else 0) + (if testBit b (2*k+22) then 1 else 0)) (2*((k !> corner (adaptedMoves ! i)) + 11)) | k <- [1..8]])

  writeTable :: IO ()
  writeTable = generateTable >>=
    writeTableToFile "Stage4.dat"

  generateTable :: IO (Table BitArr)
  generateTable = execStateT (bfs (S.singleton (toBitArr identity))) Map.empty where
    bfs :: S.Seq BitArr -> StateT (Table BitArr) IO ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM movesStage4 $ \m -> do
        let c = applyBitArr x m
        ma <- get
        if Map.member c ma
          then return []
          else modify ( Map.insert c (invert m)) >> return [c]
      let zs = xs S.>< S.fromList (concat ys)
      liftIO . putStrLn . show . length $ zs
      bfs (zs)
