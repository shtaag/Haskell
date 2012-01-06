module Points where

import Graphics.Rendering.OpenGL
import System.Random
import Data.Time

-- default example
points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin(t), cos(t), 0.0)) [1..n]

-- the width means radius
cylpoints :: Int -> Int -> Int -> [(GLfloat, GLfloat, GLfloat)]
cylpoints width height n' = 
  let n = fromIntegral n'
      -- how to decide seed???
      xs = toRandPosNeg 10 $ randNArray width n 0 
      ys = toRandPosNeg 20 $ randNArray height n 100
  in zip3 (convert xs) (convert ys) (convert (toRandPosNeg 30 $ map (\k -> calcZ k width) xs))
  
convert :: [Float] -> [GLfloat]
convert xs = map intToGLfloat xs 
  where
    intToGLfloat :: Float -> GLfloat
    intToGLfloat x = realToFrac x

randomSeq :: Int -> Int -> [Int]
randomSeq r seed = randomRs (0,r) (mkStdGen seed)

randomPNSeq :: Int -> [Int]
randomPNSeq seed = map posneg (randomSeq 10000 seed)
  where
    posneg :: Int -> Int
    -- it may not be a good implementation...
    posneg x | x >= 5000 = 1
             | x < 5000 = -1
             | otherwise = error "seed"

randNArray :: Int -> Int -> Int -> [Float]
randNArray r n seed = take n $ map fromIntegral (randomSeq r seed)

calcZ :: Float -> Int -> Float
-- incaseof failure???
calcZ x width' =  (sqrt (1 - (x^2) / (width^2))) * width
  where
    width = fromIntegral width'
    
toRandPosNeg :: Int -> [Float] -> [Float]
toRandPosNeg seed xs = [x* (fromIntegral y) | (x, y) <- zip xs (randomPNSeq seed)]

-- implemented for seed, but it is not used
getCTime = do 
  x <- getCurrentTime       
  return (mkStdGen $ truncate $ todSec $ timeToTimeOfDay $ utctDayTime x)
     
