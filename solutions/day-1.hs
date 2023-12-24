{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import System.Environment (getArgs)
import Data.Char  (isNumber)
import Data.List as L (foldl')
type Input    = [(Int, Int)]  
type Solution = Int

parser1 :: String -> [(Int, Int)]
parser1 input = go <$> lines input where 
  go :: String -> (Int, Int)
  go str = let r = parseStr str in (head r, last r)
  parseStr :: String -> [Int]
  parseStr [] = [] 
  parseStr (n:r)
    | isNumber n = read @Int [n] : parseStr r 
    | otherwise = parseStr r

parser2 :: String -> [(Int, Int)]
parser2 input = go <$> lines input where
  go :: String -> (Int, Int)
  go str = 
      let t = txtParser str 
      in (head t,  last t)
  txtParser :: String -> [Int]
  txtParser [] = [] 
  txtParser ('o':'n':'e'        :r) = 1 : txtParser ('e':r)
  txtParser ('t':'w':'o'        :r) = 2 : txtParser ('o':r)
  txtParser ('t':'h':'r':'e':'e':r) = 3 : txtParser ('e':r)
  txtParser ('f':'o':'u':'r'    :r) = 4 : txtParser r
  txtParser ('f':'i':'v':'e'    :r) = 5 : txtParser ('e':r)
  txtParser ('s':'i':'x'        :r) = 6 : txtParser r
  txtParser ('s':'e':'v':'e':'n':r) = 7 : txtParser ('n':r)
  txtParser ('e':'i':'g':'h':'t':r) = 8 : txtParser ('t':r)
  txtParser ('n':'i':'n':'e'    :r) = 9 : txtParser ('e':r)
  txtParser (c:r )
    | isNumber c = read (pure c) : txtParser r
    | otherwise  = txtParser r

solve :: Input -> Solution
solve = foldl' (\c (l,r)  -> c + (l*10) + r) 0 

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  if read @Int part == 1
    then do
      input <- parser1 <$> readFile filepath -- use parser <$> readFile filepath if String is better
      mapM_ print input
      putStrLn "solution to problem 1 is:"
      print $ solve input
    else do
      input <- readFile filepath -- use parser <$> readFile filepath if String is better
      let parsed = parser2 input
      putStrLn "solution to problem 2 is:"
      print $ solve parsed

