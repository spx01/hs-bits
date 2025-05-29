#!/usr/bin/env stack
{- stack script
   --install-ghc
   --package diagrams
   --package vector
-}

import Control.Arrow ((>>>))
import Data.Function ((&))

newtype Eoris = Eoris [Bool] deriving (Show, Eq, Ord)

newtype Coris = Coris [Int] deriving (Show, Eq, Ord)

coris0 :: Coris
coris0 = replicate 8 0 & Coris

eoris0 :: Eoris
eoris0 = replicate 12 False & Eoris

newtype Perm = Perm {unperm :: [Int]} deriving (Show, Eq, Ord)

perm0 :: Int -> Perm
perm0 = enumFromTo 1 >>> Perm

data Cube = Cube
  { edgeos :: Eoris,
    edgeps :: Perm,
    cornos :: Coris,
    cornps :: Perm
  }

cube0 :: Cube
cube0 =
  Cube
    { edgeos = eoris0,
      edgeps = perm0 12,
      cornos = coris0,
      cornps = perm0 8
    }
