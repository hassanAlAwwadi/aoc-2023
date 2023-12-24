{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Day2 where
import Text.ParserCombinators.ReadP as P
import Data.Char as C
import Data.Functor (($>), (<$))
import Data.List(foldl')
data Color
    = Red
    | Green
    | Blue
    deriving Show

example :: [String]
example =
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]


data Limit = Limit { hasRed :: Int, hasGreen :: Int, hasBlue :: Int }
type Pull = [(Int, Color)]
type Game = (Int, [Pull])
parse_game :: P.ReadP Game
parse_game     = let
    parse_pull = P.manyTill
        ((,)
            <$> (read @Int <$> P.munch1 C.isDigit)
            <*   P.skipSpaces
            <*>  P.choice [P.string "red" $> Red, P.string "green" $> Green, P.string "blue" $> Blue]
            <*   optional (char ',')
            <*   P.skipSpaces)
        (P.eof <++ (char ';' $> ()))
    in (,)
    <$ P.string "Game"
    <* P.skipSpaces
    <*> (read @Int <$> munch1 C.isDigit)
    <* char ':'
    <* skipSpaces
    <*> P.manyTill (parse_pull <* skipSpaces) P.eof

forceParse :: ReadP a -> String -> a
forceParse p s = fst . head $ readP_to_S p s

possibleGame:: Limit -> Game -> Bool
possibleGame Limit {hasRed = reds, hasBlue = blues, hasGreen = greens} = go where
    go (_, pulls) = all (all possiblePulls) pulls
    possiblePulls (n, Red)   = n <= reds
    possiblePulls (n, Blue)  = n <= blues
    possiblePulls (n, Green) = n <= greens

main_1 :: IO Int
main_1 = do
    input <- lines <$> readFile "../inputs/day-2.input"
    print $ take 10 input
    let games   = forceParse parse_game <$> input
    let possible = filter (possibleGame (Limit {hasRed = 12, hasGreen = 13, hasBlue = 14})) games
    pure $ sum $ map fst possible

cubesNeeded :: Game -> (Int, Int, Int)
cubesNeeded (_, pulls) = foldl' go (0,0,0)  pulls where
    go seed [] = seed
    go (r,g,b) ((n,Red)  :ps)= go (max n r,g,b) ps
    go (r,g,b) ((n,Green):ps)= go (r,max n g,b) ps
    go (r,g,b) ((n,Blue) :ps)= go (r,g,max n b) ps

main_2 :: IO Int
main_2 = do
    input <- lines <$> readFile "../inputs/day-2.input"
    print $ take 10 input
    let games   = forceParse parse_game <$> input
    let needed  = map cubesNeeded games
    pure $ sum $ map (\(r,g,b) -> r*g*b)  needed
