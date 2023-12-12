{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.Types (CInt)
import SDL hiding (normalize)
import SDL.Image hiding (quit)
import System.Random
import Utils
import Vec

data GameState = GameState
  { posn :: V2 Float,
    dim :: V2 Float,
    vel :: V2 Float,
    gAngle :: Float,
    objects :: [RectangleObject]
  }

-- | The main entry point.
main :: IO ()
main = do
  initializeAll
  window <- createWindow "Rectangles" (defaultWindow {windowInitialSize = V2 1900 720})
  renderer <- createRenderer window (-1) defaultRenderer
  loTextures <- mapM (\i -> 
                        putStrLn ("Loading image assets/sprites/" ++ (show i) ++ ".png...") >>
                        loadImage ("assets/sprites/" ++ (show i) ++ ".png") renderer) [1..5]
  putStrLn "Loading map..."
  putStrLn (show (length loTextures))
  rectangles <- readFileToRects "assets/levels/m2.map" loTextures
  let gameState = GameState (V2 10 10) (V2 20 20) (V2 0 0) 0 rectangles
    in appLoop renderer gameState
  destroyRenderer renderer
  destroyWindow window
  quit

-- | The main application loop. This will run until the user presses the Q key.
appLoop :: Renderer -> GameState -> IO ()
appLoop renderer gameState = do
  events <- pollEvents

  -- Query for keypressed events.
  let qPressed = any (isKeyPressed KeycodeQ) events
      pPressed = any (isKeyPressed KeycodeP) events
      leftPressed = any (isKeyPressed KeycodeLeft) events
      rightPressed = any (isKeyPressed KeycodeRight) events
      upPressed = any (isKeyPressed KeycodeUp) events
      downPressed = any (isKeyPressed KeycodeDown) events
      aPressed = any (isKeyPressed KeycodeA) events
      dPressed = any (isKeyPressed KeycodeD) events

  let d = 3
  let pw = 20
  let ph = 20

  -- If up is pressed, increase the velocity in the y direction.
  let vel' = if upPressed then V2 0 (-d) else vel gameState

  -- If down is pressed, decrease the velocity in the y direction.
  let vel'' = if downPressed then V2 0 d else vel'

  -- If left is pressed, decrease the velocity in the x direction.
  let vel''' = if leftPressed then V2 (-d) 0 else vel''

  -- If right is pressed, increase the velocity in the x direction.
  let vel'''' = if rightPressed then V2 d 0 else vel'''

  -- If we press P, stop moving.
  let vel''''' = if pPressed then V2 0 0 else vel''''

  let angleDelta = if dPressed then 1 else if aPressed then -1 else 0

  -- Fill a rectangle of size 640x480 with black color
  clear renderer
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer (Just $ Rectangle (P $ V2 0 0) (V2 1900 720))

  -- Get the RectangleObjects from the game state and draw them.
  let objs = objects gameState
  rendererDrawColor renderer $= V4 0 0 255 255
  mapM_
    ( \(RectangleObject posn dim _ _) -> do
        rendererDrawColor renderer $= (V4 255 0 0 255)
        fillRect renderer (Just $ Rectangle (P $ v2FloatToV2CInt $ posn) (v2FloatToV2CInt $ dim))
        rendererDrawColor renderer $= (V4 255 255 255 255)
        drawRect renderer (Just $ Rectangle (P $ v2FloatToV2CInt $ posn) (v2FloatToV2CInt $ dim))
    )
    objs

  ---------------------------------------------------------------------------
  -- CASTING SECTION
  ---------------------------------------------------------------------------

  -- Draw the "player".
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect
    renderer
    ( Just $
        Rectangle
          (P $ v2FloatToV2CInt $ posn gameState)
          (v2FloatToV2CInt $ dim gameState)
    )

  -- Draw the rays.
  let fov = 70
  let resolution = 1280
  let startAngle = ((gAngle gameState) - fov / 2)
  let endAngle = ((gAngle gameState) + fov / 2)
  let rays =
        map
          ( \i ->
              let rayAngle = normalize i 0 resolution startAngle endAngle
               in fireRay (posn gameState) (dim gameState) rayAngle 500 gameState
          )
          (enumFromTo 0 resolution)
  mapM_
    ( \((V3 rx ry angle), c) -> do
        rendererDrawColor renderer $= (V4 255 255 255 255)
        drawLine renderer (P $ v2FloatToV2CInt $ (offset (posn gameState) (V2 (pw / 2) (ph / 2)))) (P $ V2 rx ry)
    )
    (fmap v3FloatToV3CIntColors rays)

  let angleDelta = if dPressed then 5 else if aPressed then -5 else 0

  ---------------------------------------------------------------------------
  -- PROJECTION SECTION
  ---------------------------------------------------------------------------

  -- Draw half of the screen as blue.
  rendererDrawColor renderer $= V4 135 206 235 255
  fillRect renderer (Just $ Rectangle (P $ V2 950 0) (V2 950 360))

  -- Draw bottom half of the screen as gray.
  rendererDrawColor renderer $= V4 58 59 60 255
  fillRect renderer (Just $ Rectangle (P $ V2 950 360) (V2 950 720))

  -- Draw the pseudo-3D view. This is done by drawing one-pixel wide vertical lines
  -- of varying heights, depending on the distance from the player to the wall. 
  -- The texture is also drawn, and the coordinates (into the texture map) thereof 
  -- are computed by computing how far along the wall the ray hit.
  let len = fromIntegral (length rays)
  mapM_
    ( \((V4 i rx ry angle), mTexture) ->
        let ca = normalize angle startAngle endAngle (-35) 35
            ca' = ca * (pi / 180)
            distance = (cos ca') * (dist (posn gameState) (V2 rx ry))
            wallX = normalize i 0 len 950 1900
            wallHeight = 720 * 50 / distance
            wallY = 720 / 2 - wallHeight / 2
            imgX = if ry /= fromIntegral (floor ry) 
                    then (ry / 64 - fromIntegral (floor (ry / 64))) * 64
                   else (rx / 64 - fromIntegral (floor (rx / 64))) * 64
         in case mTexture of
              Nothing -> rendererDrawColor renderer $= V4 255 255 255 255
              Just texture -> do
                copy renderer 
                     texture 
                     (Just (SDL.Rectangle (P (V2 (floatToCInt imgX) 0)) (V2 1 64)))
                     (Just (SDL.Rectangle (P (V2 (floatToCInt wallX) (floatToCInt wallY))) (V2 1 (floatToCInt wallHeight))))

    )
    (convertV3ListToV4List rays)

  -- Update the game.
  present renderer
  SDL.delay 16
  let newGameState = (updateGameState (GameState (posn gameState) (dim gameState) vel''''' (gAngle gameState) objs) angleDelta)
  unless qPressed (appLoop renderer newGameState)

-- | Updates the game state with the given velocity.
updateGameState :: GameState -> Float -> GameState
updateGameState (GameState posn dim vel angle objs) angleDelta =
  GameState (posn + vel) dim vel (angle + angleDelta) objs

-- | Returns True if the given key is pressed in the given event.
isKeyPressed :: Keycode -> Event -> Bool
isKeyPressed key event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed
        && keysymKeycode (keyboardEventKeysym keyboardEvent) == key
    _ -> False

-- | Fires a single ray from the starting vector, using the given angle
-- | in degrees. The returned value is the coordinates of the ending point.
fireRay :: V2 Float -> V2 Float -> Float -> Float -> GameState -> (V3 Float,Maybe Texture)
fireRay (V2 px py) (V2 pw ph) angle maxDist gameState =
  let angle' = angle * (pi / 180)
      tmpEndX = px + (pw / 2) * cos angle' * maxDist
      tmpEndY = px + (ph / 2) * sin angle' * maxDist
   in case getClosestIntersectionFromAllObjects (V2 px py) (V2 tmpEndX tmpEndY) (objects gameState) of
        Nothing -> (V3 tmpEndX tmpEndY angle, Nothing)
        Just ((V2 ex ey),texture) -> (V3 ex ey angle,Just texture)

-- | Given a starting point and an ending point, and a list of RectangleObjects,
-- | compute the closest intersection point if it exists.
getClosestIntersectionFromAllObjects :: V2 Float -> V2 Float -> [RectangleObject] -> Maybe ((V2 Float),Texture)
getClosestIntersectionFromAllObjects start end objs =
  let intersections = catMaybes (map (\obj -> getClosestIntersectionPt start end obj) objs)
   in if (length intersections) == 0
        then Nothing
        else
          Just $
            minimumBy
              ( \(pt1, c1) (pt2, c2) ->
                  compare
                    (dist start pt1)
                    (dist start pt2)
              )
              intersections

-- | Given a ray starting and ending points, and a Rectangle,
-- | compute the closest intersection point if it exists.
getClosestIntersectionPt :: V2 Float -> V2 Float -> RectangleObject -> Maybe ((V2 Float),Texture)
getClosestIntersectionPt (V2 x1 y1) (V2 x2 y2) (RectangleObject pos dim texture lineSegments) =
  let intersections = catMaybes $ (map (\(line, texture) -> intersects (V4 x1 y1 x2 y2) line texture) lineSegments)
   in -- Compute the point with the minimum distance from the starting point.
      if (length intersections) == 0
        then Nothing
        else
          Just $
            minimumBy
              ( \(((V2 px1 py1, t1))) ((V2 px2 py2, t2)) ->
                  compare
                    (dist (V2 x1 y1) (V2 px1 py1))
                    (dist (V2 x1 y1) (V2 px2 py2))
              )
              intersections
