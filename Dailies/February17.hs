import Control.Monad (forM_)
import Data.List (scanl)

display :: (Int, Int) -> (Int, Int) -> IO()
display (x,y) (n, m)= do
    forM_ [0..(m-1)] $ \i -> do
        forM_ [0..(n-1)] $ \j -> do
            if x == j && y == i then
                putStr "X "
            else
                putStr "O "
        putStr "\n"

move :: Num a => (a, a) -> (a, a) -> (a, a)
move (x,y) (dx, dy) = (x + dx, y + dy)

run :: (Show a, RealFrac a) => Int -> (a, a) -> (a, a) -> IO()
run n (x, y) (dx, dy) = do
    let w = f $ floor x
        h = f $ floor y
        f b = if b `mod` 2 == 0 then 2 * b else 2 * b + 1

    let steps = scanl move (x,y) $ replicate (n-1) (dx, dy)
    -- print steps

    forM_ steps $ \(a,b) -> do
        putStr "----\t"
        print (a,b)
        display (floor a, floor b) (w, h)
        
main :: IO ()
main = do
    putStrLn "####\nn=3, pos=(3,3), spd=(1,0)"
    run 3 (3,3) (1,0)

    putStrLn "####\nn=10, pos=(7,4), spd=(0.7, 0.2)"
    run 10 (7,4) (0.7, 0.2)

    putStrLn "####\nn=14, pos=(3.7,4.9), spd=(0.1,0.2)"
    run 14 (3.7,4.9) (0.1,0.2)

    putStrLn "####\nn=12, pos=(0,0), spd=(0.5,1)"
    run 14 (3.7,4.9) (0.1,0.2)
