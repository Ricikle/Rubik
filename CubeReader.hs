module CubeReader (readCube) where
  import qualified Cube as Cube
  import Control.Monad
  import Data.List
  import Data.Array
  import qualified Data.Map.Lazy as Map

  data Colour = W | Y | B | G | R | O deriving (Show,Read,Eq,Ord,Ix)
  newtype Face = Face (Array Int Colour) deriving (Show,Eq)

  (!!!) :: Face -> Int -> Colour
  (Face f) !!! i = f ! i

  instance Ord Face where
    f1 <= f2 = f1 !!! 5 <= f2 !!! 5 

  data CornerPiece = Corner {rotationc :: Int,piecec :: Int}

  data EdgePiece = Edge {rotatione :: Int,piecee :: Int}

  cornerPieces :: Map.Map (Colour,Colour,Colour) Int
  cornerPieces = Map.fromList [((Y,G,R),1),((Y,B,R),2),((Y,B,O),3),
    ((Y,G,O),4),((W,G,R),5),((W,G,O),6),((Y,B,O),7),((W,B,R),8)]
  edgePieces :: Map.Map (Colour,Colour) Int
  edgePieces = Map.fromList [((Y,R),1),((Y,B),2),((Y,O),3),((Y,G),4),((W,R),5),((W,G),6),
    ((W,O),7),((W,B),8),((B,O),9),((B,R),10),((G,R),11),((G,O),12)]


  toFace :: String -> Face
  toFace xs = Face $ listArray (1,9) $ map read (words xs)

  toCorner :: (Colour, Colour, Colour) -> CornerPiece
  toCorner (first, second, third) = case biggest of
      1 -> Corner 0 $ cornerPieces Map.! (if second > third then (first,second,third) else (first,third,second))
      2 -> Corner 2 $ cornerPieces Map.! (if first > third then (second,first,third) else (second,third,first))
      3 -> Corner 1 $ cornerPieces Map.! (if first > second then (third,first,second) else (third,second,first))
    where biggest = if first > second then if first > third then 1 else 3 else if second >third then 2 else 3

  toEdge :: (Colour, Colour) -> EdgePiece
  toEdge (f, s) = if f > s then
    Edge 0 $ edgePieces Map.! (f,s)
    else Edge 1 $ edgePieces Map.! (s,f)

  readCube:: IO (Cube.Cube)
  readCube = do
    putStrLn "Enter faces of cube (red towards and yellow facing upwards)"
    faces <- sequence (replicate 6 getLine)
    let sorted = listArray (W,O) $ sort $ map toFace faces
        cornerList = map toCorner [
          (sorted ! Y !!! 9, sorted ! G !!! 1, sorted ! R !!! 3),
          (sorted ! Y !!! 7, sorted ! R !!! 1, sorted ! B !!! 3),
          (sorted ! Y !!! 1, sorted ! B !!! 1, sorted ! O !!! 3),
          (sorted ! Y !!! 3, sorted ! G !!! 3, sorted ! O !!! 1),
          (sorted ! W !!! 3, sorted ! R !!! 9, sorted ! G !!! 7),
          (sorted ! W !!! 9, sorted ! G !!! 9, sorted ! O !!! 7),
          (sorted ! W !!! 7, sorted ! O !!! 9, sorted ! B !!! 7),
          (sorted ! W !!! 1, sorted ! B !!! 9, sorted ! R !!! 7)
          ]
        edgeList = map toEdge [
          (sorted ! Y !!! 8 , sorted ! R !!! 2),
          (sorted ! Y !!! 4 , sorted ! B !!! 2),
          (sorted ! Y !!! 2 , sorted ! O !!! 2),
          (sorted ! Y !!! 6 , sorted ! G !!! 2),
          (sorted ! W !!! 2 , sorted ! R !!! 8),
          (sorted ! W !!! 6 , sorted ! G !!! 8),
          (sorted ! W !!! 8 , sorted ! O !!! 8),
          (sorted ! W !!! 4 , sorted ! B !!! 8),
          (sorted ! B !!! 4 , sorted ! O !!! 6),
          (sorted ! B !!! 6 , sorted ! R !!! 4),
          (sorted ! G !!! 4 , sorted ! R !!! 6),
          (sorted ! G !!! 6 , sorted ! O !!! 4)
          ]
        corner = Cube.Arr $ array (1,8) $ zip (map piecec cornerList) [1..8]
        edge = Cube.Arr $ array (1,12) $ zip (map piecee edgeList) [1..12]
    return $ Cube.Cube corner edge (Cube.fromList $ map rotationc cornerList) (Cube.fromList $ map rotatione edgeList)
