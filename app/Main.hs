module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle

screenWidth   = 500
screenHeight  = 500
windowOffsetX = 100
windowOffsetY = 100

maxBoidSpeed  = 6

data World = World {
    worldBound :: (Float, Float),
    iterations :: Int,
    boids      :: [Boid]
}

data Boid = Boid {
    colorB   :: Color,
    position :: Vector,
    velocity :: Vector,
    procRang :: Float,
    visRange :: Float
}

addV :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

subV :: Vector -> Vector -> Vector
subV (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

randomColor :: IO Color
randomColor = do
    r <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    g <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    b <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    return $ makeColorI r g b 255

window :: Display
window = InWindow "Boids: Haskell" (screenWidth, screenHeight) (windowOffsetX, windowOffsetY)

background :: Color
background = dim $ light black

drawing :: World -> Picture 
drawing w = pictures [ 
    uncurry translate (worldBound w) $ scale 0.1 0.1 $ color red $ text $ show (iterations w),
    pictures $ map boidDraw (boids w),
    scale 2.0 2.0 $ color (light black) $ uncurry rectangleWire (worldBound w)
                    ]

timestep :: ViewPort -> Float -> World -> World
timestep v t w = World (worldBound w) (iterations w + 1) (map (boidBound (worldBound w) . boidUpdate) (boids w))

makeBoid :: IO Boid
makeBoid = do
    c  <- randomColor

    px <- randomRIO (-100, 100)
    py <- randomRIO (-100, 100)

    vx <- randomRIO (-1, 1)
    vy <- randomRIO (-1, 1)

    return $ Boid c (px, py) (normalizeV (vx, vy)) 10 25

boidDraw :: Boid -> Picture
boidDraw b = pictures [ 
                uncurry translate (addV (position b) (8, 0)) $
                scale 0.05 0.05 $ 
                color (colorB b) $
                text (show (velocity b)),

                uncurry translate (position b) $ 
                color (colorB b) $ 
                rotate (270 + radToDeg (uncurry atan2 (velocity b))) $
                polygon [ (10, 0), (0, 3.5), (3, 0), (0, -3.5) ]   
                    ]                                    

boidUpdate :: Boid -> Boid
boidUpdate b 
    | magV (velocity b) > maxBoidSpeed = Boid (colorB b) (addV (position b) (velocity b)) (velocity b) (procRang b) (visRange b)
    | otherwise                        = Boid (colorB b) (addV (position b) (velocity b)) (subV (velocity b) (0, -0.001)) (procRang b) (visRange b)

boidBound :: (Float, Float) -> Boid -> Boid
boidBound (w, h) b
    | w  < fst (position b) = Boid (colorB b) (-w + (fst (position b) - w), snd $ position b) (velocity b) (procRang b) (visRange b)
    | -w > fst (position b) = Boid (colorB b) (w - (fst (position b) + w), snd $ position b)  (velocity b) (procRang b) (visRange b)
    | h  < snd (position b) = Boid (colorB b) (fst $ position b, -h + (snd (position b) - h)) (velocity b) (procRang b) (visRange b)
    | -h > snd (position b) = Boid (colorB b) (fst $ position b, h - (snd (position b) + h))  (velocity b) (procRang b) (visRange b)
    | otherwise = b

main :: IO ()
main = do
    boids <- replicateM 5 makeBoid
    let world = World (250, 250) 0 boids

    mapM_ (print . magV . velocity) boids 
    putStrLn ""
    mapM_ (print . velocity) boids

    simulate window background 60 world drawing timestep
