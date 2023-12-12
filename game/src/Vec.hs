module Vec where

import SDL hiding (normalize)
import Data.List
import Data.Word
import Prelude
import Foreign.C.Types (CInt)
import Utils

-- | Converts a V2 Float to a V2 CInt.
v2FloatToV2CInt :: V2 Float -> V2 CInt
v2FloatToV2CInt (V2 x y) = V2 (floatToCInt x) (floatToCInt y)

-- | Converts a V3 Float pair containing a possible texture to a V3 CInt.
v3FloatToV3CIntColors :: (V3 Float, Maybe Texture) -> (V3 CInt, Maybe Texture)
v3FloatToV3CIntColors ((V3 x y angle), texture) = ((V3 (floatToCInt x) (floatToCInt y) (floatToCInt angle)),texture)

-- | Determines whether or not two line segments intersect. If so, returns the point of intersection.
-- | This algorithm comes straight from Java's Line2D class.
intersects :: V4 Float -> V4 Float -> Texture -> Maybe (V2 Float, Texture)
intersects (V4 x1 y1 x2 y2) (V4 x3 y3 x4 y4) color
    | x1 == x2 && y1 == y2 || x3 == x4 && y3 == y4 = Nothing
    | denominator == 0 || ua < 0 || ua > 1 || ub < 0 || ub > 1 = Nothing
    | otherwise = Just (V2 x y, color)
  where
    denominator = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
    ua = num1 / denominator
    ub = num2 / denominator
    num1 = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
    num2 = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
    x = x1 + ua * (x2 - x1)
    y = y1 + ua * (y2 - y1)

-- | Determines the Euclidean distance between two points.
dist :: V2 Float -> V2 Float -> Float
dist (V2 x1 y1) (V2 x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- | Converts a value from one range to another.
normalize :: Float -> Float -> Float -> Float -> Float -> Float
normalize n oldMin oldMax newMin newMax =
    ((n - oldMin) * (newMax - newMin) / (oldMax - oldMin)) + newMin

-- | Converts a list of (V3,V4) pairs to (V4,V4).
convertV3ListToV4List :: [(V3 Float, Maybe Texture)] -> [(V4 Float, Maybe Texture)]
convertV3ListToV4List v3List = zipWith (\index ((V3 x y angle), c) -> ((V4 index x y angle), c)) [0..] v3List
