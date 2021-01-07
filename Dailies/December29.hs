-- Beginner
import Data.HashMap.Strict
import Data.Hashable

invertMap :: (Ord k, Ord v, Hashable k, Hashable v) => HashMap k v -> HashMap v k
invertMap xs = fromList $ zip (fmap (snd) y) (fmap (fst) y)
    where y = toList xs


-- quadraticSequence :: [Float] -> [Float]
-- quadraticSequence [] = []
-- quadraticSequence (x:y:z:xs) = 
--     where d2 = (z - y) - (y - x) -- calculate second difference
--           d1 = (y - x) -- calculate first difference

-- main :: IO ()
-- main = do
--     let a = 