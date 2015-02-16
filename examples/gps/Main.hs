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
    -- If it has changed, send a chat message with the position and the
    -- type of block they are standing on.
    if pos /= oldPos then do
        let x = getx pos
            y = gety pos
            z = getz pos
        block <- worldGetBlock(x, y-1, z)
        chatPost(show x ++ " " ++ show y ++ " " ++
                 show z ++ " "++ show block)
        return ()
    else
        return()
    loop (if pos /= oldPos then pos else oldPos)
  where
    getx (x,y,z) = x
    gety (x,y,z) = y
    getz (x,y,z) = z
