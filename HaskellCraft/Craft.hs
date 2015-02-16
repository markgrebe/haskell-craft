{-# LANGUAGE FlexibleInstances, GADTs, KindSignatures,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module HaskellCraft.Craft where

import           Control.Applicative
import           Control.Monad (ap, liftM2)

import           Data.Monoid
import           Data.Text.Lazy.Builder

import           HaskellCraft.Block
import           HaskellCraft.Parser

import           Prelude hiding (Show)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)

-----------------------------------------------------------------------------

type Tile = (Int, Int, Int)
type Pos  = (Double, Double, Double)

data Craft :: * -> * where
        Method    :: Method                      -> Craft ()
        Query     :: T.Show a => Query a         -> Craft a
        Bind      :: Craft a -> (a -> Craft b)   -> Craft b
        Return    :: a                           -> Craft a

instance Monad Craft where
        return = Return
        (>>=) = Bind

instance Applicative Craft where
  pure  = return
  (<*>) = ap

instance Functor Craft where
  fmap f c = c >>= return . f

instance Monoid a => Monoid (Craft a) where
  mappend = liftM2 mappend
  mempty  = return mempty

-- Mincraft Pi API methods with no return values
data Method
        = WorldSetBlock (Int, Int, Int, Block)
        | WorldSetBlockWithData (Int, Int, Int, Block, Int)
        | WorldSetBlocks (Int, Int, Int, Int, Int, Int, Block)
        | WorldSetBlocksWithData (Int, Int, Int, Int, Int, Int, Block, Int)
        | WorldCheckpointSave
        | WorldCheckpointRestore
        | WorldSetting (WorldSettingT, Bool)
        | ChatPost String
        | PlayerSetTile (Int, Int, Int)
        | PlayerSetPos (Double, Double, Double)
        | PlayerSetting (PlayerSettingT, Bool)
        | EntitySetTile (Int, Int, Int, Int)
        | EntitySetPos (Int, Double, Double, Double)
        | CameraSetNormal (Int)
        | CameraSetFollow (Int)
        | CameraSetFixed
        | CameraSetPos (Double, Double, Double)
        | EventsClear

instance S.Show Method where
  showsPrec p = (++) . toString . showbPrec p

instance T.Show Method where
  showb (WorldSetBlock (x,y,z,b)) = "world.setBlock("
         <> showb x <> singleton ',' <> showb y <> singleton ','
         <> showb z <> singleton ',' <> showb (fromEnum b) <> ")\n"
  showb (WorldSetBlockWithData (x,y,z,b,d)) = "world.setBlock("
         <> showb x <> singleton ',' <> showb y <> singleton ','
         <> showb z <> singleton ',' <> showb (fromEnum b) <> singleton ','
         <> showb d <> ")\n"
  showb (WorldSetBlocks (x1,y1,z1,x2,y2,z2,b)) = "world.setBlocks("
         <> showb x1 <> singleton ',' <> showb y1 <> singleton ','
         <> showb z1 <> singleton ',' <> showb x2 <> singleton ','
         <> showb y2 <> singleton ',' <> showb z2 <> singleton ','
         <> showb (fromEnum b) <> ")\n"
  showb (WorldSetBlocksWithData (x1,y1,z1,x2,y2,z2,b,d)) = "world.setBlocks("
         <> showb x1 <> singleton ',' <> showb y1 <> singleton ','
         <> showb z1 <> singleton ',' <> showb x2 <> singleton ','
         <> showb y2 <> singleton ',' <> showb z2 <> singleton ','
         <> showb (fromEnum b) <> singleton ',' <> showb d <> ")\n"
  showb WorldCheckpointSave = "world.checkpoint.save()\n"
  showb WorldCheckpointRestore = "world.checkpoint.restore()\n"
  showb (WorldSetting (ws, b)) = "world.setting("
         <> showb ws <> if b then "1)\n" else "0)\n"
  showb (ChatPost s) = "chat.post(" <> showb s <> ")\n"
  showb (PlayerSetTile (x,y,z)) = "player.setTile("
         <> showb x <> singleton ',' <> showb y <> singleton ','
         <> showb z <> ")\n"
  showb (PlayerSetPos (x,y,z)) = "player.setPos("
         <> showb x <> singleton ',' <> showb y <> singleton ','
         <> showb z <> ")\n"
  showb (PlayerSetting (ps, b)) = "player.setting("
         <> showb ps <> if b then "1)\n" else "0)\n"
  showb (EntitySetTile (i,x,y,z)) = "entity.setTile("
         <> showb i <> singleton ',' <> showb x <> singleton ','
         <> showb y <> singleton ',' <> showb z <> ")\n"
  showb (EntitySetPos (i,x,y,z)) = "entity.setPos("
         <> showb i <> singleton ',' <> showb x <> singleton ','
         <> showb y <> singleton ',' <> showb z <> ")\n"
  showb (CameraSetNormal (i)) = "camera.setNormal("
         <> showb i <> ")\n"
  showb (CameraSetFollow (i)) = "camera.setFollow("
         <> showb i <> ")\n"
  showb CameraSetFixed = "camera.setFixed()\n"
  showb (CameraSetPos (x,y,z)) = "camera.setPos("
         <> showb x <> singleton ',' <> showb y <> singleton ','
         <> showb z <> ")\n"
  showb EventsClear = "events.clear()\n"

data WorldSettingT
        = WorldImmutable
        | NametagsVisible

instance T.Show WorldSettingT where
  showb WorldImmutable = "world_immutable"
  showb NametagsVisible = "nametags_visible"

data PlayerSettingT
        = Autojump

instance T.Show PlayerSettingT where
  showb Autojump = "autojump"

worldSetBlock :: (Int, Int, Int, Block) -> Craft ()
worldSetBlock = Method . WorldSetBlock

worldSetBlockWithData :: (Int, Int, Int, Block, Int) -> Craft ()
worldSetBlockWithData = Method . WorldSetBlockWithData

worldSetBlocks :: (Int, Int, Int, Int, Int, Int, Block) -> Craft ()
worldSetBlocks = Method . WorldSetBlocks

worldSetBlocksWithData :: (Int, Int, Int, Int, Int, Int, Block, Int) -> Craft ()
worldSetBlocksWithData = Method . WorldSetBlocksWithData

worldCheckpointSave :: () -> Craft ()
worldCheckpointSave () = Method WorldCheckpointSave

worldCheckpointRestore :: () -> Craft ()
worldCheckpointRestore () = Method WorldCheckpointRestore

worldSetting :: (WorldSettingT, Bool) -> Craft ()
worldSetting = Method . WorldSetting

playerSetTile :: (Int, Int, Int) -> Craft ()
playerSetTile = Method . PlayerSetTile

playerSetPos :: (Double, Double, Double) -> Craft ()
playerSetPos = Method . PlayerSetPos

playerSetting :: (PlayerSettingT, Bool) -> Craft ()
playerSetting = Method . PlayerSetting

chatPost :: String -> Craft ()
chatPost = Method . ChatPost

entitySetTile :: (Int, Int, Int, Int) -> Craft ()
entitySetTile = Method . EntitySetTile

entitySetPos :: (Int, Double, Double, Double) -> Craft ()
entitySetPos = Method . EntitySetPos

cameraSetNormal :: (Int) -> Craft ()
cameraSetNormal = Method . CameraSetNormal

cameraSetFollow :: (Int) -> Craft ()
cameraSetFollow = Method . CameraSetFollow

cameraSetFixed :: () -> Craft ()
cameraSetFixed () = Method CameraSetFixed

cameraSetPos :: (Double, Double, Double) -> Craft ()
cameraSetPos = Method . CameraSetPos

eventsClear :: () -> Craft ()
eventsClear () = Method EventsClear

-----------------------------------------------------------------------------

-- Mincraft Pi API methods with return values
data Query :: * -> * where
        WorldGetBlock :: (Int, Int, Int) -> Query Block
        WorldGetBlockWithData :: (Int, Int, Int) -> Query (Block, Int)
        WorldGetHeight :: (Int, Int) -> Query Int
        WorldGetPlayerIds :: Query [Int]
        PlayerGetTile :: Query (Int, Int, Int)
        PlayerGetPos :: Query (Double, Double, Double)
        EntityGetTile :: Int -> Query (Maybe (Int, Int, Int))
        EntityGetPos :: Int -> Query (Maybe (Double, Double, Double))
        EventsBlockHits :: Query [(Int, Int, Int, Int, Int)]

instance S.Show (Query a) where
  showsPrec p = (++) . toString . showbPrec p

instance T.Show (Query a) where
  showb (WorldGetBlock (x,y,z)) = "world.getBlock(" <> showb x <> singleton ','
                                                    <> showb y <> singleton ','
                                                    <> showb z <> ")\n"
  showb (WorldGetBlockWithData (x,y,z)) = "world.getBlockWithData("
                                                    <> showb x <> singleton ','
                                                    <> showb y <> singleton ','
                                                    <> showb z <> ")\n"
  showb (WorldGetHeight (x,z)) = "world.getHeight(" <> showb x <> singleton ','
                                                    <> showb z <> ")\n"
  showb WorldGetPlayerIds = "world.getPlayerIds()\n"
  showb PlayerGetTile = "player.getTile()\n"
  showb PlayerGetPos = "player.getPos()\n"
  showb (EntityGetTile i) = "entity.getTile(" <> showb i <> ")\n"
  showb (EntityGetPos i) = "entity.getPos(" <> showb i <> ")\n"
  showb EventsBlockHits = "events.block.hits()\n"

-- This is how we take our value to bits
parseQueryResult :: Query a -> String -> a
parseQueryResult (WorldGetBlock {}) o         = toEnum $ parseOneInt o
parseQueryResult (WorldGetBlockWithData {}) o = parseBlockIntCSV o
parseQueryResult (WorldGetHeight {}) o        = parseOneInt o
parseQueryResult (WorldGetPlayerIds {}) o     = parseIntList o
parseQueryResult (PlayerGetTile {}) o         = parseThreeCSVInts o
parseQueryResult (PlayerGetPos {}) o          = parseThreeCSVFloats o
parseQueryResult (EntityGetTile {}) o         = parseMaybeThreeCSVInts o
parseQueryResult (EntityGetPos {}) o          = parseMaybeThreeCSVFloats o
parseQueryResult (EventsBlockHits {}) o       = parseEventList o

worldGetBlock :: (Int, Int, Int) -> Craft Block
worldGetBlock = Query . WorldGetBlock

worldGetBlockWithData :: (Int, Int, Int) -> Craft (Block, Int)
worldGetBlockWithData = Query . WorldGetBlockWithData

worldGetHeight :: (Int, Int) -> Craft Int
worldGetHeight = Query . WorldGetHeight

worldGetPlayerIds :: () -> Craft [Int]
worldGetPlayerIds () = Query WorldGetPlayerIds

playerGetTile :: () -> Craft (Int, Int, Int)
playerGetTile () = Query PlayerGetTile

playerGetPos :: () -> Craft (Double, Double, Double)
playerGetPos () = Query PlayerGetPos

entityGetTile :: Int -> Craft (Maybe (Int, Int, Int))
entityGetTile = Query . EntityGetTile

entityGetPos :: Int -> Craft (Maybe (Double, Double, Double))
entityGetPos = Query . EntityGetPos

eventsBlockHits :: () -> Craft [(Int, Int, Int, Int, Int)]
eventsBlockHits () = Query EventsBlockHits
