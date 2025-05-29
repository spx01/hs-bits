#!/usr/bin/env stack
{- stack script
   --install-ghc
   --snapshot lts-22.44
   --package vector
   --package containers
   --package bytestring-trie
   --package bytestring
-}

{-
  Read and decode SOC-1 microcode.
  https://ocw.cs.pub.ro/courses/soc/laboratoare/09
-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow (first, second, (>>>))
import Control.Monad (guard)
import Data.Bits
import Data.ByteString qualified as B
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (find, intercalate)
import Data.Set qualified as S
import Data.String
import Data.Trie qualified as TR
import Data.Vector qualified as V
import System.IO (readFile')
import Text.Printf (printf)

type Line = S.Set String

newtype Rom = Rom {unrom :: V.Vector String}

readRom :: FilePath -> IO Rom
readRom f = do
  c <- readFile' f
  let r =
        lines
          >>> fmap (filter (`elem` "01"))
          >>> filter (not . null)
          >>> V.fromList
          >>> Rom
          $ c
  guard $ all (\s -> length s == length ctlFlagsOg) $ unrom r
  pure r

class Insn a where
  insnIdx :: a -> Int

instance Insn Int where
  insnIdx = id

insnNameTrie :: TR.Trie Int
insnNameTrie = first fromString <$> insnNames & TR.fromList

lookupInsnPrefix :: String -> [Int]
lookupInsnPrefix s = TR.submap (fromString s) insnNameTrie & TR.elems

lookupInsnExact :: String -> Maybe Int
lookupInsnExact = fromString >>> (`TR.lookup` insnNameTrie)

instance Insn String where
  insnIdx s
    | Just u <- lookupInsnExact s' = u
    | [u] <- lookupInsnPrefix (s' ++ " ") = u
    | [u] <- lookupInsnPrefix s' = u
    | otherwise = error "non-unique instruction prefix"
    where
      s' = toLower <$> s

type UStep = S.Set String

bin2steps :: String -> UStep
bin2steps s = S.fromList $ fmap snd $ filter (\(c, _) -> c == '1') $ zip s ctlFlagsOg

getSteps' :: (Insn a) => a -> Rom -> V.Vector UStep
getSteps' (insnIdx -> i) = (bin2steps <$>) . V.slice (i * stageCount) stageCount . unrom

newtype PrettySteps = PS {unps :: V.Vector UStep}

vTakeUntil1 :: (a -> Bool) -> V.Vector a -> V.Vector a
vTakeUntil1 p xs =
  fmap fst $
    V.takeWhile ((< 1) . snd) $
      V.zip xs (V.scanl (\count x -> if p x then count + 1 else count) 0 xs)

instance Show PrettySteps where
  show (PS v) = f <$> V.indexed v' & V.toList & unlines
    where
      f (idx, S.toList -> l) = printf "%2d %s" idx (intercalate " | " l)
      v' =
        vTakeUntil1 (S.member "SIG_END") v
          & fmap (S.map (dropWhile (/= '_') >>> drop 1))

getSteps :: (Insn a) => a -> Rom -> PrettySteps
getSteps i = getSteps' i >>> PS

dumpRom :: Rom -> [(String, PrettySteps)]
dumpRom r = insnNames & fmap (second $ flip getSteps r)

main :: IO ()
main = do
  rom <- readRom "rom.bin"
  let d = dumpRom rom
  d
    & fmap (\(s, ps) -> s ++ ":\n" ++ show ps)
    & intercalate "\n"
    & putStr

ctlFlagsOg :: [String]
ctlFlagsOg =
  reverse
    [ "SIG_RET",
      "SIG_CALL",
      "SIG_RAM_ENL",
      "SIG_RAM_ENH",
      "SIG_RAM_LOAD",
      "SIG_MDR_EN",
      "SIG_MDR_LOAD",
      "SIG_MAR_LOADL",
      "SIG_MAR_LOADH",
      "SIG_PC_EN",
      "SIG_PC_LOAD",
      "SIG_PC_INC",
      "SIG_IR_LOAD",
      "SIG_ALU_EN",
      "SIG_ALU_LD",
      "SIG_ALU_OP0",
      "SIG_ALU_OP1",
      "SIG_ALU_OP2",
      "SIG_ALU_OP3",
      "SIG_FLAGS_LDC",
      "SIG_FLAGS_LDB",
      "SIG_FLAGS_LDA",
      "SIG_C_DEC",
      "SIG_C_INC",
      "SIG_C_EN",
      "SIG_C_LOAD",
      "SIG_B_DEC",
      "SIG_B_INC",
      "SIG_B_EN",
      "SIG_B_LOAD",
      "SIG_A_DEC",
      "SIG_A_INC",
      "SIG_A_EN",
      "SIG_A_LOAD",
      "IO_EN",
      "IO_LD",
      "IOA_LD",
      "SIG_HLT",
      "SIG_END"
    ]

insnNames :: [(String, Int)]
insnNames =
  [ ("add b", 0x80),
    ("add c", 0x81),
    ("ana b", 0xA0),
    ("ana c", 0xA1),
    ("ani byte", 0xE6),
    ("call addr", 0xCD),
    ("cma", 0x2F),
    ("dcr a", 0x3D),
    ("dcr b", 0x05),
    ("dcr c", 0x0D),
    ("hlt", 0x76),
    ("inr a", 0x3C),
    ("inr b", 0x04),
    ("inr c", 0x0C),
    ("jmp addr", 0xC3),
    ("jm addr", 0xFA),
    ("jnz addr", 0xC2),
    ("jz addr", 0xCA),
    ("lda addr", 0x3A),
    ("mov a, b", 0x78),
    ("mov a, c", 0x79),
    ("mov b, a", 0x47),
    ("mov b, c", 0x41),
    ("mov c, a", 0x4F),
    ("mov c, b", 0x48),
    ("mvi a, byte", 0x3E),
    ("mvi b, byte", 0x06),
    ("mvi c, byte", 0x0E),
    ("nop", 0x00),
    ("ora b", 0xB0),
    ("ora c", 0xB1),
    ("ori byte", 0xF6),
    ("ral", 0x17),
    ("rar", 0x1F),
    ("ret", 0xC9),
    ("sta addr", 0x32),
    ("sub b", 0x90),
    ("sub c", 0x91),
    ("xra b", 0xA8),
    ("xra c", 0xA9),
    ("xri byte", 0xEE)
  ]

stageBits :: Int
stageBits = 4

stageCount :: Int
stageCount = 1 .<<. stageBits
