module DrawLine where

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
        if m > 1.0 || m < -1.0 then
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
