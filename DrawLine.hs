module DrawLine where

data Point = Point Float Float deriving (Show)

getRange :: Int -> Int -> [Int]
getRange a b
    | a <= b = [a..b]
    | otherwise = [b..a]

getSlope :: Point -> Point -> Float
getSlope (Point x1 y1) (Point x2 y2) = (y2-y1)/(x2-x1)

drawLine :: Point -> Point -> [Point]
drawLine f@(Point fx fy) l@(Point lx ly)
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
                        (m * (fromIntegral x - fx) + fy)
                    )
                )
                (getRange (round fx) (round lx))
        where
            m= getSlope f l
