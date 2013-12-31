module DrawLine where

import Data.List (foldl')

data Point = Point Float Float deriving (Show)

getRange :: Int -> Int -> [Int]
getRange a b
    | a <= b = [a..b]
    | otherwise = [b..a]
{-|
 - Get the slope of a line described by the two points received as arguments
 -}
getSlope :: Point -> Point -> Float
getSlope (Point x1 y1) (Point x2 y2) = (y2-y1)/(x2-x1)

{-|
 - Get a list of points where pixels should be filled to draw a line
 - (Inspired from Bresenham's line algorithm)
 - Takes two Point arguments representing the end points of the line, the points
 - will be located on a line between these two points
 -}
getLinePoints :: Point -> Point -> [Point]
getLinePoints f@(Point fx fy) l@(Point lx ly)
    -- vertical line
    | fx == lx = [(Point fx (fromIntegral y)) | y <- getRange (round fy) (round ly)]
    | otherwise =
        -- generate points by mapping on the "longer side" of the line
        if m > 1.0 || m < -1.0 then
            -- the rise if higher than the run
            map (\y ->
                    (Point
                        ((1/m) * ((fromIntegral y) - fy) + fx)
                        (fromIntegral y)
                    )
                )
                (getRange (round fy) (round ly))
        else
            map (\x ->
                    (Point
                        (fromIntegral x)
                        (m * ((fromIntegral x) - fx) + fy)
                    )
                )
                (getRange (round fx) (round lx))
        where
            m = getSlope f l

{-|
 - Given two lists of points, first on the X-axis, second on the Y-axis return
 - the coefficients of the first grade equation that approximates the data set
 -
 - In other words get a linear trend for the data set
 -
 - The elements of the returned pair (a,b) map like this to the linear equation:
 - f(x) = ax + b
 -
 - This function is using the ordinary least squares method to determine the
 - line equation
 - See: https://en.wikipedia.org/wiki/Ordinary_least_squares
 -
 - TODO: length xs == 0, xSquareSum == 0 (xs == [0..0]), xSquareSum*(length xs) - xSum^2 == 0
 -}
trendLine :: (Num a, Fractional a) => [a] -> [a]-> (a, a)
trendLine xs ys = (a, b)
    where
        xSum = foldl' (+) 0 xs
        ySum = foldl' (+) 0 ys
        xSquaredSum = foldl' (+) 0 $ map (^2) xs
        xySum = foldl' (+) 0 $ zipWith (*) xs ys

        b = ((xSquaredSum*ySum) - (xSum*xySum))/((xSquaredSum * fromIntegral (length xs)) - xSum^2)
        a = (xySum - xSum*b)/xSquaredSum
