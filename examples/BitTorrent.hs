{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module BitTorrent where

import Control.Monad
import Test.QuickCheck

import SessionCheck

data BTState = BT { choked :: Bool, interested :: Bool } deriving (Ord, Eq, Show)

data Torrent = T { numPieces :: Int
                 , totalSize :: Int } deriving (Ord, Eq, Show)

data BTMessage = Keepalive
               | Choke
               | Unchoke
               | Interested
               | NotInterested
               | Have Int
               | Request Int Int Int
               | Piece [Int]
               | Cancel Int Int Int
               | Stop
               deriving (Ord, Eq, Show)

bitfieldMessage :: Torrent -> Predicate [Bool]
bitfieldMessage t = Predicate { apply     = (numPieces t ==) . length 
                              , satisfies = replicateM (numPieces t) arbitrary
                              , name      = "bitfieldMessage " ++ show t }

peer :: ([Bool] :< t, BTMessage :< t) => Torrent -> Spec t ()
peer t = do
  bitfield <- get (bitfieldMessage t)
  go bitfield
  where
    go bitfield = undefined
    {- A sequence of legal sends which are reasonable given the other peers bitfield,
     - terminating with a Stop message-}
    {- Need to somehow agree with the current state, need some form of way of working
       with a global BTState -}

bitTorrent :: ([Bool] :< t, BTMessage :< t) => Torrent -> Spec t ()
bitTorrent t = do
  async $ peer t
  async $ dual (peer t)
