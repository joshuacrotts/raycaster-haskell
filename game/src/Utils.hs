module Utils where

import SDL hiding (normalize)
import SDL.Image hiding (quit)
import Foreign.C.Types (CInt)
import Data.Word
import Data.Char

data RectangleObject = RectangleObject { rectPosn :: V2 Float, rectDim :: V2 Float, image :: Texture, lineSegments :: [(V4 Float,Texture)] }

-- | Converts a Float into a CInt using rounding.
floatToCInt :: Float -> CInt
floatToCInt x = fromIntegral (floor x)

-- | Converts a Float into a Word8 using rounding.
floatToWord8 :: Float -> Word8
floatToWord8 x = fromIntegral (round x)

-- | Converts a CInt into a Float.
cIntToFloat :: CInt -> Float
cIntToFloat x = fromIntegral x

-- | Given a starting point and an offset vector, compute the new point.
offset :: V2 Float -> V2 Float -> V2 Float
offset (V2 x y) (V2 dx dy) = V2 (x + dx) (y + dy)

-- | Clamps a value to a range.
clamp :: Ord a => a -> a -> a -> a
clamp n min max = if n < min then min else if n > max then max else n

-- | Darkens a color by a fixed amount.
darken :: V4 Word8 -> V4 Word8
darken (V4 r g b a) = V4 (clamp (r - 50) 0 255) (clamp (g - 50) 0 255) (clamp (b - 50) 0 255) a

-- v4IntToV4Word8 :: V4 Int -> V4 Word8
-- v4IntToV4Word8 (V4 r g b a) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- Load an image
loadImage :: FilePath -> Renderer -> IO Texture
loadImage path renderer = do
    surface <- load path
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    return texture

-- | Given a tile index position in a 2D grid, the type of a tile, and the texture map,
-- | create the corresponding RectangleObject instance.
createTile :: Int -> Int -> Char -> [Texture] -> RectangleObject
createTile i j c loTextures = 
    let x = fromIntegral (i * 64)
        y = fromIntegral (j * 64)
        dim = 64
        img = (loTextures !! (((ord c) - 1) - 48)) 
    in case c of
        '1' -> RectangleObject (V2 x y) (V2 dim dim) img (getLineSegments' x y dim dim img)
        '2' -> RectangleObject (V2 x y) (V2 dim dim) img (getLineSegments' x y dim dim img)
        '3' -> RectangleObject (V2 x y) (V2 dim dim) img (getLineSegments' x y dim dim img)
        '4' -> RectangleObject (V2 x y) (V2 dim dim) img (getLineSegments' x y dim dim img)
        otherwise -> RectangleObject (V2 0 0) (V2 0 0) img []

-- | Read a file and return its contents as a list of RectangleObject instances.
readFileToRects :: FilePath -> [Texture] -> IO [RectangleObject]
readFileToRects path loTextures = do
    contents <- readFile path 
    let linesOfFile = lines contents
        numRows = length linesOfFile
        numCols = length (head linesOfFile)
        rects = (concat (zipWith (\line col -> 
                                            (zipWith (\ch row -> createTile row col ch loTextures) 
                                                     line [0..])) 
                                         linesOfFile [0..]))

    return rects

-- | Given a starting point and an ending point, compute the 
-- | four surrounding line segments of the rectangle. We also pass
-- | in the texture so that we can use it later.
getLineSegments' :: Float -> Float -> Float -> Float -> Texture -> [(V4 Float,Texture)]
getLineSegments' x' y' w' h' texture = 
    [(V4 x' y' x' (y' + h'),texture),
      (V4 (x' + w') y' (x' + w') (y' + h'),texture),
      (V4 x' y' (x' + w') y',texture),
      (V4 x' (y' + h') (x' + w') (y' + h'),texture)]
