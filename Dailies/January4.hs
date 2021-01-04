import Data.Char (ord, chr, digitToInt)
import Data.List (sort)

-- Beginner 
check :: Ord a => [a] -> String
check [] = "neither"
check xs
  | checkInc xs == True           = "increasing"
  | checkInc $ reverse xs == True = "decreasing"
  | otherwise                     = "neither"
  where checkInc []       = False
        checkInc [y]      = False
        checkInc (z:y:ys) = z < y && checkInc (y:ys)

data Growth = Decreasing | Increasing | Neither deriving (Eq, Show)
check' :: Ord a => [a] -> Growth
check' lat | lat == sortedLat         = Increasing
           | lat == reverse sortedLat = Decreasing
           | otherwise                = Neither
           where sortedLat = sort lat

-- Intermediate
type Point a = (a,a) 
data Triangle a = Triangle (Point a) (Point a) (Point a) deriving (Show, Eq)

withinTriangle :: (Ord a, Num a) => (Point a) -> (Triangle a) -> Bool
withinTriangle (px, py) (Triangle (p0x, p0y) (p1x, p1y) (p2x, p2y))
  | (s < 0) /= (t < 0)  = False
  | a < 0               = (s <= 0 && (s + t) >= a)
  | otherwise           = (s >= 0 && (s + t) <= a)
  where s = (p0y * p2x) - (p0x * p2y) + (p2y - p0y) * px + (p0x - p2x) * py
        t = (p0x * p1y) - (p0y * p1x) + (p0y - p1y) * px + (p1x - p0x) * py
        a = -p1y * p2x + p0y * (p2x - p1x) + p0x * (p1y - p2y) + p1x * p2y

-- Expert
bishop :: String -> String -> Int -> Bool
bishop origin@[xCoord, yCoord] target moves
  | moves > 1         = True
  | otherwise         = target `elem` reachable
  where (newX, newY)  = ((ord $ xCoord) - 96, digitToInt $ yCoord)
        reachable     = [ (chr $ x + 96) : show y | x <- [1..8], y <- [1..8], (abs $ newX - x) == (abs $ newY - y)]


main :: IO [()]
main = do

  -- Beginner
  print "Beginner Task"
  traverse (print) $ check <$> [[1,2,3], [3,2,1], [1,2,1], [], [1]]

  -- Intermediate
  -- Test whether a series of points are withing the same triangle
  print "Intermediate Task"
  traverse (print) $ withinTriangle <$> [(4, 5), (3, 2)] <*> [(Triangle (1, 4) (5, 6) (6, 1))]

  -- Expert
  print "Expert Task"
  traverse (print) $ bishop <$> ["c5"] <*> ["b4", "b5", "f3", "f8"] <*> [0, 1, 2]