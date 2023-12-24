{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Day9 where
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Control.Monad.Cont as C

import Data.Maybe(fromMaybe)
import Data.List(foldl', intersect, sort, scanl')
import Data.Functor (($>))
import GHC.Read (readField)

forceParse :: P.ReadP a -> String -> a
forceParse p s = fst . head $ P.readP_to_S p s

parse_line :: P.ReadP [Int]
parse_line = P.manyTill
    ( read <$> P.choice
        [ P.munch1 C.isDigit <* P.skipSpaces
        , (:) <$> P.char '-' <*> P.munch1 C.isDigit <* P.skipSpaces
        ] )
    P.eof

solve_seq :: [Int] -> Int
solve_seq []     = 0
solve_seq (x:xs) = let
    diff_list = zipWith (-) xs (x:xs)
    in case diff_list of
        []  -> x
        [d] -> d + last (x:xs)
        (d:ds) -> if all (==d) ds
            then d + last (x:xs)
            else let d' = solve_seq (d:ds) in d' + last (x:xs)


main_1 :: IO ()
main_1 = do
    input <- lines <$>  readFile "./inputs/day-9.input"
    let seqs   = map (forceParse parse_line) input
    let seqs_n = map solve_seq seqs
    print $ sum seqs_n

solve_prev :: [Int] -> Int
solve_prev []     = 0
solve_prev (x:xs) = let
    diff_list = zipWith (-) xs (x:xs)
    in case diff_list of
        []  -> x
        [d] -> x - d
        (d:ds) -> if all (==d) ds
            then x - d
            else let d' = solve_prev (d:ds) in x - d'

main_2 :: IO ()
main_2 = do
    input <- lines <$>  readFile "./inputs/day-9.input"
    let seqs   = map (forceParse parse_line) input
    let seqs_n = map solve_prev seqs
    print $ sum seqs_n
