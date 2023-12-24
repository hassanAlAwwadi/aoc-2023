{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Day5 where
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import Data.List(foldl', intersect, sort, scanl')
import Data.Functor (($>))
forceParse :: P.ReadP a -> String -> a
forceParse p s = fst . head $ P.readP_to_S p s

data Almonac = AL 
    { seeds :: [Int]
    , s_t_s :: [(Int, Int, Int)]
    , s_t_f :: [(Int, Int, Int)]
    , f_t_w :: [(Int, Int, Int)]
    , w_t_l :: [(Int, Int, Int)]
    , l_t_t :: [(Int, Int, Int)]
    , t_t_h :: [(Int, Int, Int)]
    , h_t_l :: [(Int, Int, Int)]
    } deriving (Eq, Ord, Show)

parse_almonac :: P.ReadP Almonac
parse_almonac = AL
    <$> parse_seeds 
    <*> (sort <$> parse_mapping "eed-to-soil map:")
    <*> (sort <$> parse_mapping "oil-to-fertilizer map:")
    <*> (sort <$> parse_mapping "ertilizer-to-water map:")
    <*> (sort <$> parse_mapping "ater-to-light map:")
    <*> (sort <$> parse_mapping "ight-to-temperature map:")
    <*> (sort <$> parse_mapping "emperature-to-humidity map:")
    <*> (sort <$> parse_mapping "umidity-to-location map:") 
    where 
        parse_seeds     = P.string "seeds:" 
            *> P.skipSpaces 
            *> P.manyTill 
                (read <$> P.munch1 C.isDigit <* P.skipSpaces) 
                (P.satisfy (not . C.isDigit))
        parse_mapping s = group_by_3
            <$ P.string s 
            <* P.skipSpaces 
            <*> P.manyTill 
                (read <$> P.munch1 C.isDigit <* P.skipSpaces)
                (P.choice [P.satisfy (not . C.isDigit) $> (), P.eof])

        -- also flips destination and source
        -- because that makes the ordering make more sense
        group_by_3 [] = [] 
        group_by_3 (x:y:z:r) = (y,x,z): group_by_3 r


get_mapping :: [(Int,Int,Int)] -> Int  -> Int 
get_mapping []                  n  = n 
get_mapping ((srs, drs, rl):ms) n  = if 
    | n < srs                 -> n 
    | n > srs && n < srs + rl -> drs + (n - srs)
    | otherwise               -> get_mapping ms n 

main_1 = do 
    input <- readFile "./inputs/day-5.input"
    --print input
    let al = forceParse parse_almonac input
    let init_seeds = seeds al
    let map_list = 
            [ s_t_s al 
            , s_t_f al 
            , f_t_w al 
            , w_t_l al 
            , l_t_t al 
            , t_t_h al 
            , h_t_l al 
            ]
    let transformed_seeds = foldl' (\ss trs -> map (get_mapping trs) ss) init_seeds map_list

    print $ minimum transformed_seeds
