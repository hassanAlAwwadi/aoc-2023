{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
module Day4 where
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import Data.List(foldl', intersect)

forceParse :: P.ReadP a -> String -> a
forceParse p s = fst . head $ P.readP_to_S p s

data Card = Card {
    day  :: Int,
    wins :: [Int],
    owns :: [Int]
} deriving (Eq, Ord, Show)

example :: [String]
example =
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]

parse_card :: P.ReadP Card
parse_card = Card
    <$  P.string "Card"
    <*  P.skipSpaces
    <*> (read <$> P.munch1 C.isDigit)
    <*  P.char ':'
    <*  P.skipSpaces
    <*> P.manyTill (read <$> P.munch1 C.isDigit <* P.skipSpaces) (P.char '|')
    <*  P.skipSpaces
    <*> P.manyTill (read <$> P.munch1 C.isDigit <* P.skipSpaces) P.eof

main_1 :: IO ()
main_1 = do
    input <- lines <$>  readFile "./inputs/day-4.input"
    let cards = forceParse parse_card <$> input
    let won   = map length <$> filter (not . null) $ (\(Card _ w o) -> w `intersect` o) <$> cards
    let winnings = map (\n -> ((^) @Int) 2  (n-1) ) won
    print $ sum winnings

main_2 :: IO ()
main_2 = do
    input <- lines <$>  readFile "./inputs/day-4.input"
    let cards = forceParse parse_card <$> input
    let initial_copy_count = M.empty
    let final_copy_count   = foldl' go initial_copy_count cards
    print $ sum (M.elems final_copy_count) + length cards where

        go :: M.Map Int Int -> Card -> M.Map Int Int
        go  m (Card k w o) = let
                extra = fromMaybe 0 (m M.!? k)
                matching = length $ w `intersect` o
                m' = foldl'
                    (flip (M.alter (\case Nothing -> Just (1+extra) ; Just n  -> Just (n + 1 + extra))))
                    m [k+1 .. k + matching]
                in m'