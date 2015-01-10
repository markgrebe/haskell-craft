{-# LANGUAGE CPP, GADTs, OverloadedStrings, ScopedTypeVariables #-}

-- | blank-canvas is a Haskell binding to the complete HTML5 Canvas
--   API. blank-canvas allows Haskell users to write, in Haskell,
--   interactive images onto their web browsers. blank-canvas gives
--   the users a single full-window canvas, and provides many
--   well-documented functions for rendering images.

module HaskellCraft where

import           Control.Monad

-- ToDo #if !(MIN_VERSION_base(4,8,0))
-- import           Data.Monoid (mempty)
-- #endif
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.IO hiding (hGetLine)
import           Network.Socket hiding (send)
import           System.IO hiding (hPutStrLn)

import           HaskellCraft.Craft

import           Prelude hiding (show)

-- | blankCanvas is the main entry point into blank-canvas.
-- A typical invocation would be
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >module Main where
-- >
-- >import HaskellCraft
-- >
-- >main = do
-- >       hand <- openCraft
-- >       send hand $ do
-- >                moveTo(50,50)
-- >                lineTo(200,100)
-- >                lineWidth 10
-- >                strokeStyle "red"
-- >                stroke()
-- >

openCraft :: HostName -> String -> IO Handle
openCraft hostname port = do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    return h

closeCraft :: Handle -> IO ()
closeCraft crafth = hClose crafth

-- | Sends a set of Minecraft commands to the Pi. Attempts
-- to common up as many commands as possible.

send :: Handle -> Craft a -> IO a
send hand commands =
      send' hand commands mempty
  where
      sendBind :: Handle -> Craft a -> (a -> Craft b) -> Builder -> IO b
      sendBind h (Return a)      k cmds = send' h (k a) cmds
      sendBind h (Bind m k1)    k2 cmds = sendBind h m (\ r -> Bind (k1 r) k2) cmds
      sendBind h (Method cmd)    k cmds = send' h (k ()) cmds
      sendBind h (Query query)   k cmds = sendQuery h query k cmds

      sendQuery :: Handle -> Query a -> (a -> Craft b) -> Builder -> IO b
      sendQuery h query k cmds = do
          sendToCraft h cmds
          s <- hGetLine h
          send' h (k (parseQueryResult query s)) mempty

      send' :: Handle -> Craft a -> Builder -> IO a
      -- Most of these can be factored out, except return
      send' h (Bind m k)            cmds = sendBind h m k cmds
      send' _ (Return a)            cmds = do
              sendToCraft hand cmds
              return a
      send' h cmd                   cmds = sendBind h cmd Return cmds

-- | internal command to send a message to the canvas.
sendToCraft :: Handle -> Builder -> IO ()
sendToCraft hand cmds = do
    hPutStrLn hand (toLazyText cmds)
    hFlush hand

testIt :: IO Int
testIt = do
    ch <- openCraft "192.168.200.107" "4711"
    b <- send ch $ do
       b <- worldGetBlock (20, 0, -10)
       return b
    closeCraft ch
    return b

