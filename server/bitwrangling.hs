#!/usr/bin/env stack
-- stack --resolver nightly-2016-02-18 --install-ghc runghc --package formatting --package text --package binary --package bytestring
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BinaryLiterals #-}
import           Text.Printf
import           Formatting
import           Data.Bits (complement)
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Binary.Get

{- TO EXECUTE:

$ stack bitwrangling.hs

-}


example :: IO ()
example = do
  fprint (bin % "\n") 5
  -- 101
  let i :: Int = -5
  putStrLn $ printf "%d in binary (64 bit): %b" i i
  -- -5 in binary (64 bit): 1111111111111111111111111111111111111111111111111111111111111011
  let i :: Word8 = -5
  putStrLn $ printf "%d (unsigned) in binary: %b" i i
  -- 251 (unsigned) in binary: 11111011
  let i :: Word8 = (complement 5) + 1  -- twos complement
  putStrLn $ printf "%d (unsigned) in binary: %b" i i
  -- 251 (unsigned) in binary: 11111011
  fprint ("via formatting: " % bin % "\n") i
  -- via formatting: 11111011
  fprint (int % "\n") 0b10001001  -- drive opcode
  -- 137
  fprint (int % "\n") 0x89        -- drive opcode
  -- 137

bytestringsToBits :: IO ()
bytestringsToBits = do
  let b :: ByteString = "\x80\x83"
      ws = BS.unpack b
  mapM_ (fprint (bin % "\n")) ws
  putStrLn "----------"
  mapM_ print ws
  -- 10000000
  -- 10000011
  -- 128 : start opcode
  -- 131 : safe opcode
  putStrLn " stop: "
  let b :: ByteString = "\x89\x00\x00\x00\x01"
      ws = BS.unpack b
  -- pad with 0 until the full Word8 has been satisfied (won't work for twos complement)
  -- TODO(MAZUMDER) is 9 a bug? or a usage problem?
  mapM_ (fprint (left 9 '0' %. bin % "\n")) ws
  putStrLn "----------"
  mapM_ print ws
  --  10001001
  --  00000000
  --  00000000
  --  00000000
  --  00000001
  --  ----------
  --  137  : drive opcode
  --  0
  --  0
  --  0
  --  1

forward15 :: IO ()
forward15 = do
  let b :: ByteString = "\x89\x00\x96\x80\x00"
      ws = BS.unpack b
  -- TODO(MAZUMDER) is 9 a bug? or a usage problem?
  mapM_ (fprint (left 9 '0' %. bin % "\n")) ws
  putStrLn "----------"
  mapM_ print ws

backward15 :: IO ()
backward15 = do
  let b :: ByteString = "\x89\xffj\x80\x00"
      ws = BS.unpack b
  -- TODO(MAZUMDER) is 9 a bug? or a usage problem?
  mapM_ (fprint (left 9 '0' %. bin % "\n")) ws
  putStrLn "----------"
  mapM_ print ws
  -- let driveop : highv : lowv : highr : lowr : [] = ws
  -- http://stackoverflow.com/a/23412736 word8 to word16
  -- print highv
  --incorrect
  -- let x = runGet getWord16le "\xffj"
  -- print x -- 27391
  -- print (complement x + 1) -- 38145
  let x = runGet getWord16be "\xffj"
  print x -- 65386
  print (complement x + 1) -- 150 (so 65386 is -150 mm/s)
  print (-150 :: Word16)

  let radius = runGet getWord16be "\x80\x00"
  print radius -- 32768
  let straight = 32768 :: Word16
  print straight
  fprint (bin % "\n") straight




main :: IO ()
main = bytestringsToBits
