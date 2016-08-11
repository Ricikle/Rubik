module Cube where

  import Data.Array

  data Move = U | U' | U2 | L | L' | L2 | R | R' | R2 | D | D' | D2 | B | B' | B2 | F | F' | F2 deriving (Show,Read,Eq)
  moves :: [Move]
  moves = [U, U', U2, L, L', L2, R, R', R2, D, D', D2, B, B', B2, F, F', F2]
  identity :: Cube
  identity = Cube (fromCycle 8 []) (fromCycle 12 []) (fromList zero8) (fromList zero12)

  type Cycle = [Int]
  type SetSize = Int
  newtype Arr = Arr {getArr :: Array Int Int} deriving (Eq,Ord)
  instance Show Arr where
    show (Arr a) = show [a!k | k <- let (_,b) = bounds a in [1..b]]
  instance Read Arr where
    readsPrec _ str = let (l,rest):_  = ((readsPrec 0 str) ::[([Int],String)]) in [(Arr (listArray (1,length l) l),rest)]
  type Permutation = Arr
  type Orientation = Arr
  data Cube = Cube {corner :: Permutation,
                    edges :: Permutation,
                    cornerO :: Orientation,
                    edgeO :: Orientation } deriving (Show)

  maxN :: Arr -> Int
  maxN (Arr a) = snd $ bounds a

  apply :: [Move] -> Cube -> Cube
  apply [] c = c
  apply (x:xs) c = let c' = case x of
                              U -> u c
                              U'-> u' c
                              U2-> u2 c
                              L -> l c
                              L'-> l' c
                              L2-> l2 c
                              R -> r c
                              R'-> r' c
                              R2-> r2 c
                              D -> d c
                              D'-> d' c
                              D2-> d2 c
                              B -> b c
                              B'-> b' c
                              B2-> b2 c
                              F -> f c
                              F'-> f' c
                              F2-> f2 c

    in apply xs c'

  invert :: Move -> Move
  invert U = U'
  invert L = L'
  invert R = R'
  invert D = D'
  invert B = B'
  invert F = F'
  invert U' = U
  invert L' = L
  invert R' = R
  invert D' = D
  invert B' = B
  invert F' = F
  invert x = x

  infixl 8 !>
  (!>) :: Int -> Permutation -> Int
  x !> (Arr p) = p ! x

  fromCycle :: SetSize -> [Cycle] -> Permutation
  fromCycle n xss = Arr $ listArray (1,n) $ [searchCycles (xss) k | k <- [1..]] where
    searchCycles [] k = k
    searchCycles (ys:yss) k = let k' = searchOneCycle ys k (head ys) in if k' == k then searchCycles yss k else k'
    searchOneCycle [] k _ = k
    searchOneCycle (y:ys) k first = if y == k then if null ys then first else head ys else searchOneCycle ys k first

  fromList :: [Int] -> Orientation
  fromList xs = let n = length xs in Arr $ listArray (1,n) xs

  toList :: Arr -> [Int]
  toList arr = [getArr arr ! k | k <- [1..maxN arr]]

  join :: Permutation -> Permutation -> Permutation
  x1 `join` x2 = let n = maxN x1 in Arr $ listArray (1,n) [k !> x1 !> x2 | k <-[1..n]]

  zero8,zero12 :: [Int]
  zero8 = replicate 8 0
  zero12 = replicate 12 0

  move :: [Cycle] -> [Cycle] -> [Int] -> [Int] -> Cube -> Cube
  move pc' pe' o1' o2' x = Cube c e co eo where
    pc = fromCycle 8 pc'
    pe = fromCycle 12 pe'
    o1 = fromList o1'
    o2 = fromList o2'
    c = corner x `join` pc
    e = edges x `join` pe
    co = Arr $ array (1,8) [(k !> pc , (getArr (cornerO x) ! k + getArr o1 ! k) `mod` 3) | k <- [1..8]]
    eo = Arr $ array (1,12) [(k !> pe , (getArr (edgeO x) ! k + getArr o2 ! k) `mod` 2)  | k <- [1..12]]

  u,u',u2,d,d',d2,f,f',f2,b,b',b2,l,l',l2,r,r',r2 :: Cube -> Cube
  u = move [[1,2,3,4]] [[1,2,3,4]] zero8 zero12
  u' = move [[1,4,3,2]] [[1,4,3,2]] zero8 zero12
  u2 = move [[1,3],[2,4]] [[1,3],[2,4]] zero8 zero12
  d = move [[5,6,7,8]] [[5,6,7,8]] zero8 zero12
  d' = move [[5,8,7,6]] [[5,8,7,6]] zero8 zero12
  d2 = move [[5,7],[6,8]] [[5,7],[6,8]] zero8 zero12
  f = move [[1,5,8,2]] [[1,11,5,10]] [1,2,0,0,2,0,0,1] zero12
  f' = move [[1,2,8,5]] [[1,10,5,11]] [1,2,0,0,2,0,0,1] zero12
  f2 = move [[1,8],[5,2]] [[1,5,11,10]] zero8 zero12
  b = move [[3,7,6,4]] [[3,9,7,12]] [0,0,2,1,0,2,1,0] zero12
  b' = move [[3,4,6,7]] [[3,12,7,9]] [0,0,2,1,0,2,1,0] zero12
  b2 = move [[3,6],[4,7]] [[3,7],[9,12]] zero8 zero12
  l = move [[2,8,7,3]] [[2,10,8,9]] [0,1,2,0,0,0,1,2] [0,1,0,0,0,0,0,1,1,1,0,0]
  l' = move [[2,3,7,8]] [[2,9,8,10]] [0,1,2,0,0,0,1,2] [0,1,0,0,0,0,0,1,1,1,0,0]
  l2 = move [[2,7],[3,8]] [[2,8],[9,10]] zero8 zero12
  r = move [[1,4,6,5]] [[4,12,6,11]] [2,0,0,1,1,2,0,0] [0,0,0,1,0,1,0,0,0,0,1,1]
  r' = move [[1,5,6,4]] [[4,11,6,12]] [2,0,0,1,1,2,0,0] [0,0,0,1,0,1,0,0,0,0,1,1]
  r2 = move [[1,6],[4,5]] [[4,6],[11,12]] zero8 zero12
