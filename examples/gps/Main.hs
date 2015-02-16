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
    oldPos <- send ch $ do
        pos <- playerGetTile ()
        return pos
    loop ch oldPos

loop :: Handle -> (Int, Int, Int) -> IO ()
loop ch oldPos = do
    -- Get players new position
    pos <- send ch $ playerGetTile()
    -- If it has changed, send a chat message with the position and the
    -- type of block they are standing on.
    if pos /= oldPos then do
        let x = getx pos
            y = gety pos
            z = getz pos
        send ch $ do
            block <- worldGetBlock(x, y-1, z)
            chatPost(show x ++ " " ++ show y ++ " " ++
                     show z ++ " "++ show block)
            return ()
    else
        return()
    loop ch (if pos /= oldPos then pos else oldPos)
  where
    getx (x,y,z) = x
    gety (x,y,z) = y
    getz (x,y,z) = z
