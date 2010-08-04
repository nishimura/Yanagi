module Yanagi.Random (genList, genStr) where

import Control.Applicative
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek,sizeOf)
import System.IO (openBinaryFile,IOMode(ReadMode),hGetBuf,hClose)
import System.Random (Random, mkStdGen, StdGen, randomRs)

genList :: (Random a) => (a, a) -> Int -> IO [a]
genList range len = take len <$> randomRs range <$> betterStdGen

betterStdGen :: IO StdGen
betterStdGen = alloca $ \p -> do
    h <- openBinaryFile "/dev/urandom" ReadMode
    _ <- hGetBuf h p $ sizeOf (undefined :: Int)
    hClose h
    mkStdGen <$> peek p

genStr :: Int -> IO String
genStr len = map toChar <$> take len <$> randomRs (0, 62) <$> betterStdGen

toChar :: Int -> Char
toChar a
    | a < 10 = toEnum $ a + 48
    | a < 36 = toEnum $ a + 55
    | a < 62 = toEnum $ a + 61
    | otherwise = 'a'
