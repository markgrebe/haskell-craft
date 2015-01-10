{-# LANGUAGE FlexibleInstances, GADTs, KindSignatures,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module HaskellCraft.Craft where

import           Control.Applicative
import           Control.Monad (ap, liftM2)

import           Data.Monoid
import           Data.Text.Lazy.Builder

import           Prelude hiding (Show)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)

-----------------------------------------------------------------------------

data Craft :: * -> * where
        Method    :: Method                      -> Craft ()     -- <context>.<method>
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
        = WorldSetBlock (Int, Int, Int, Int)
        | WorldSetBlockWithData (Int, Int, Int, Int, Int)
        | WorldCheckpointSave
        | WorldCheckpointRestore

instance S.Show Method where
  showsPrec p = (++) . toString . showbPrec p

instance T.Show Method where
  showb (WorldSetBlock (i1,i2,i3,i4)) = "world.setBlock("
         <> showb i1 <> singleton ',' <> showb i2 <> singleton ','
         <> showb i3 <> singleton ',' <> showb i4 <> singleton ')'
  showb (WorldSetBlockWithData (i1,i2,i3,i4,i5)) = "world.setBlockWithData("
         <> showb i1 <> singleton ',' <> showb i2 <> singleton ','
         <> showb i3 <> singleton ',' <> showb i4 <> singleton ','
         <> showb i5 <> singleton ')'
  showb WorldCheckpointSave = "world.checkpoint.save()"
  showb WorldCheckpointRestore = "world.checkpoint.restore()"

-----------------------------------------------------------------------------

-- Mincraft Pi API methods with return values
data Query :: * -> * where
        WorldGetBlock :: (Int, Int, Int) -> Query Int

instance S.Show (Query a) where
  showsPrec p = (++) . toString . showbPrec p

instance T.Show (Query a) where
  showb (WorldGetBlock (x,y,z)) = "world.getBlock(" <> showb x <> singleton ','
                                                    <> showb y <> singleton ','
                                                    <> showb z <> singleton ')'

-- This is how we take our value to bits
parseQueryResult :: Query a -> String -> a
parseQueryResult (WorldGetBlock {}) o         = read o

worldGetBlock :: (Int, Int, Int) -> Craft Int
worldGetBlock = Query . WorldGetBlock
