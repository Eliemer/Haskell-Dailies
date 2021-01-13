-- Beginner, Intermediate, and Expert in Haskell


-- Beginner
myMax :: RealFrac a => a -> a -> a
myMax a b = if c > d then a else b
  where getDecimal = snd . properFraction
        c = getDecimal a
        d = getDecimal b

-- Intermediate
-- maxSum :: RealFrac a => a -> a -> a


-- Expert
higherRotation :: (Ord a, Floating a) => a -> a -> a -> a -> (a,a) -- 4 args, 1 res
higherRotation x y angle scalar = (if x'' > epsilon || x'' < (-epsilon) then scalar * x'' else 0, if y'' > epsilon || y'' < (-epsilon) then scalar * y'' else 0)
  where epsilon = 1e-8
        mag = sqrt $ x ** 2 + y ** 2 
        -- Normalize the vector
        -- (x', y')
        --     | mag /= 0 = (x/mag, y/mag)
        --     | otherwise = (0, 0)
        angle'     = angle * (pi/180)
        res@(x'', y'') = ((x * cos angle') - (y * sin angle'), (x * sin angle') + (y * cos angle'))

rotate :: (Floating a) => a -> (a,a) -> (a,a)
rotate agl vec@(x,y) = ((x * cos agl) - (y * sin agl), (x * sin agl) + (y * cos agl))