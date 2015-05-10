module Main (main) where

import HaskellCraft
import HaskellCraft.Block
import HaskellCraft.Craft

import System.IO

main :: IO ()
main = do
    -- Open a connection to the Minecraft game
    ch <- openCraft "192.168.200.107" "4711"
    -- Get the initial player position
    send ch $ do
        pos <- playerGetTile ()
        loop pos

loop :: (Int, Int, Int) -> Craft ()
loop oldPos = do
    -- Get players new position
    pos <- playerGetTile()
    -- If it has changed, Plant a flower where we are.
    if pos /= oldPos then do
        let x = getx pos
            y = gety pos
            z = getz pos
        worldSetBlock(x,y,z,Flower_yellow)
        return ()
    else
        return()
    loop (if pos /= oldPos then pos else oldPos)
  where
    getx (x,y,z) = x
    gety (x,y,z) = y
    getz (x,y,z) = z
