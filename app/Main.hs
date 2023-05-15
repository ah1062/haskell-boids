module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle

screenWidth   = 500
screenHeight  = 500
windowOffsetX = 100
windowOffsetY = 100

turnFactor,visualRange,protectedRange,centerFactor,avoidFactor,matchingFactor,minBoidSpeed,maxBoidSpeed,maxBias,biasIncrement,biasVal :: Float
turnFactor     = 0.05
visualRange    = 75.0
protectedRange = 15.0
centerFactor   = 0.005
avoidFactor    = 0.005
matchingFactor = 0.03
minBoidSpeed   = 1.0
maxBoidSpeed   = 2.0
maxBias        = 0.01
biasIncrement  = 0.00004
biasVal        = 0.001


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
} deriving (Eq, Show)

addV :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

subV :: Vector -> Vector -> Vector
subV (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

averageV :: [Vector] -> Vector
averageV [] = (0,0)
averageV vs = mulSV (1 / fromIntegral (length vs)) (foldr addV (0,0) vs)

distance :: Vector -> Vector -> Float
distance (x1, y1) (x2, y2) = sqrt ( (x2 - x1) ** 2 + (y2 - y1) ** 2 )

randomColor :: IO Color
randomColor = do
    r <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    g <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    b <- randomRIO (round 0.3 * 255, round 0.7 * 255)
    return $ makeColorI r g b 180

window :: Display
window = InWindow "Boids: Haskell" (screenWidth, screenHeight) (windowOffsetX, windowOffsetY)

background :: Color
background = dim $ light black

drawing :: World -> Picture 
drawing w = pictures [ 
    scale 2.0 2.0 $ color (dark white) $ uncurry rectangleWire (worldBound w),
    
    -- Draw All Boids
    pictures $ map (boidDraw w) (boids w),

    -- Draw Average Boid Position
    uncurry translate (averageV (map position (boids w))) $ color white $ circleSolid 10
                    ]

timestep :: ViewPort -> Float -> World -> World
timestep v t w = World (worldBound w) (iterations w + 1) (map (boidBound (worldBound w) . boidUpdate (boids w)) (boids w))

makeBoid :: IO Boid
makeBoid = do
    c  <- randomColor

    px <- randomRIO (-200, 200)
    py <- randomRIO (-200, 200)

    vx <- randomRIO (-1, 1)
    vy <- randomRIO (-1, 1)

    sp <- randomRIO (minBoidSpeed, maxBoidSpeed)

    return $ Boid c (px, py) (mulSV sp $ normalizeV (vx, vy)) protectedRange visualRange

boidDraw :: World -> Boid -> Picture
boidDraw w b = pictures [ 
                --uncurry translate (position b) $ color (dark red) $ circle (procRang b),
                --uncurry translate (position b) $ color (dark green) $ circle (visRange b),

                --uncurry translate (addV (position b) (8, 0)) $
                --scale 0.05 0.05 $ 
                --color (colorB b) $
                --text (show (boidSeparation b (boids w))),

                uncurry translate (position b) $ 
                color (colorB b) $ 
                rotate (270 + radToDeg (uncurry atan2 (velocity b))) $
                polygon [ (5, 0), (-5, 3.5), (-2, 0), (-5, -3.5) ]   
                    ]                                    

boidUpdate :: [Boid] -> Boid -> Boid
boidUpdate bs b = Boid {
    colorB   = colorB b,
    position = addV (position b) (velocity b),
    velocity = foldr addV (0,0) [velocity b, boidSepVel, boidAliVel],
    procRang = procRang b,
    visRange = visRange b
                    }
      where separationVector = boidSeparation b bs
            alignmentVectors = boidAlignment b bs
            cohesionVectors  = boidCohesion b bs
            boidSepVel       = mulSV avoidFactor separationVector
            boidAliVel       = mulSV matchingFactor (subV (averageV alignmentVectors) (velocity b))
            boidCohVel       = mulSV centerFactor (subV (averageV cohesionVectors) (position b))

boidSeparation :: Boid -> [Boid] -> Vector
boidSeparation b [] = (0,0)
boidSeparation b bs
    | b == head bs                                            = addV (0,0) (boidSeparation b (tail bs))
    | distance (position b) (position $ head bs) > procRang b = addV (0,0) (boidSeparation b (tail bs))
    | otherwise                                               = addV ( subV (position b) (position (head bs)) ) (boidSeparation b (tail bs))

boidAlignment :: Boid -> [Boid] -> [Vector]
boidAlignment b [] = []
boidAlignment b bs
    | b == head bs                                            = boidAlignment b (tail bs)
    | distance (position b) (position $ head bs) > visRange b = boidAlignment b (tail bs)
    | distance (position b) (position $ head bs) < procRang b = boidAlignment b (tail bs)
    | otherwise                                               = velocity (head bs) : boidAlignment b (tail bs)

boidCohesion :: Boid -> [Boid] -> [Vector]
boidCohesion b [] = []
boidCohesion b bs
    | b == head bs                                            = boidCohesion b (tail bs)
    | distance (position b) (position $ head bs) > visRange b = boidCohesion b (tail bs)
    | distance (position b) (position $ head bs) < procRang b = boidCohesion b (tail bs)
    | otherwise                                               = position (head bs) : boidCohesion b (tail bs)

boidBound :: (Float, Float) -> Boid -> Boid
boidBound (w, h) b    
    | w-50  < fst (position b)         = Boid (colorB b) (position b) (addV (-turnFactor, 0) (velocity b)) (procRang b) (visRange b)
    | -w+50 > fst (position b)         = Boid (colorB b) (position b) (addV (turnFactor, 0)  (velocity b)) (procRang b) (visRange b)
    | h-50  < snd (position b)         = Boid (colorB b) (position b) (addV (0, -turnFactor) (velocity b)) (procRang b) (visRange b)
    | -h+50 > snd (position b)         = Boid (colorB b) (position b) (addV (0, turnFactor)  (velocity b)) (procRang b) (visRange b)
    | minBoidSpeed > magV (velocity b) = Boid (colorB b) (position b) (mulSV minBoidSpeed $ normalizeV (velocity b)) (procRang b)  (visRange b)
    | maxBoidSpeed < magV (velocity b) = Boid (colorB b) (position b) (mulSV maxBoidSpeed $ normalizeV (velocity b)) (procRang b)  (visRange b)
    | otherwise = b

main :: IO ()
main = do
    boids <- replicateM 120 makeBoid
    let world = World (250, 250) 0 boids

    simulate window background 60 world drawing timestep
