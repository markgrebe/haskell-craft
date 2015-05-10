module Main (main) where

import HaskellCraft
import HaskellCraft.Block
import HaskellCraft.Craft
import HaskellCraft.Utils

import System.IO

import Debug.Trace

colors :: [Int]
colors = [14, 1, 4, 5, 3 , 11, 10]

height :: Double
height = 15.0

len :: Double
len = 32.0

main :: IO ()
main = do
    -- Open a connection to the Minecraft game
    ch <- openCraft "192.168.200.107" "4711"
    send ch $ do
        -- Get the initial player position
        pos <- playerGetTile ()
        let x = vector3x pos
            y = vector3y pos
            z = vector3z pos
        -- Erase the rectangle where the rainbow will go
        worldSetBlocks(x+1,y,z+1,x+truncate(len)+1,y+truncate(height),z+1,Air)
        -- Draw the rainbow slightly offset from player position
        rainbow (x+1) y (z+1) 0.0

-- Draw one vertical segment of the rainbow recursively
bowsegment :: Int -> Int -> Int -> Int -> Craft()
bowsegment x y z c = do
    worldSetBlockWithData(x,y+c,z,Wool,colors!!c)
    if c < (length colors) - 1 then
        bowsegment x y z (c+1)
    else
        return()

-- Draw the rainbow recursively
rainbow :: Int -> Int -> Int -> Double -> Craft ()
rainbow x y z t = do
    let dy = truncate $ sin(t/len * pi) * height
    bowsegment (x+truncate(t)) (y+dy) z 0
    if t < len then
        rainbow x y z (t+1.0)
    else
       return ()
