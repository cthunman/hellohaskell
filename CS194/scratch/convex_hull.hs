
data Direction where
  Left :: Direction
  Right :: Direction
  Straight :: Direction

type Point = (Double, Double)

point :: Double -> Double -> Point
point x y = (x, y)

-- angleBetweenPoints :: Point -> Point -> Double
