{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Day3 where
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Set   as S
import qualified Data.Map.Strict as M
import Data.Maybe(mapMaybe)
import Data.List(foldl')
import Data.Functor (($>), (<$))
import Text.Read(readMaybe)
import Control.Monad(forM, forM_)
import Debug.Trace

forceParse :: P.ReadP a -> String -> a
forceParse p s = fst . head $ P.readP_to_S p s

example :: [String]
example =
    [ "467..114.."
    , "...*......"
    , "..35..633."
    , "......#..."
    , "617*......"
    , ".....+.58."
    , "..592....."
    , "......755."
    , "...$.*...."
    , ".664.598.."
    ]

data ParserEnv = PE
    { line :: Int
    , char :: Int
    , uniq :: Int
    , symb :: M.Map (Int, Int) Char
    , vals :: M.Map (Int, Int) Int
    , seen :: M.Map Int Int
    }
    deriving (Show)

initial_env :: ParserEnv
initial_env = PE
    { line = 0
    , char = 0
    , uniq = 0
    , symb = M.empty
    , vals = M.empty
    , seen = M.empty
    }


type ParserState = S.State ParserEnv

consume_dots :: String -> ParserState ()
consume_dots ds = do
    p@PE { char = cc} <- S.get
    S.put p{ char = cc  + length ds}

consume_symb :: Char -> ParserState ()
consume_symb s = do
    p@PE{ line = cl, char = cc, symb = cs } <- S.get
    S.put p{ char = cc  + 1, symb = M.insert (cl, cc) s cs }

consume_numb :: String -> ParserState ()
consume_numb n = do
    p@PE{ line = cl, char = cc, uniq = u, vals = cv, seen = s} <- S.get

    let space = length n
    let intn  = read @Int n
    let xterr = [cc .. cc + space - 1]
    let nv = foldl' (\m x -> M.insert (cl, x) u m) cv xterr
    let ns = M.insert u intn s

    S.put p{ char = cc  + space, uniq = u+1,  vals = nv, seen = ns}

consume_nl :: ParserState ()
consume_nl = do
    p@PE{ line = cl } <- S.get
    S.put p{ line = cl + 1, char = 0 }

(>*>) :: P.ReadP (ParserState a) -> P.ReadP (ParserState b) -> P.ReadP (ParserState b)
(>*>) pa pb = (*>)
    <$> pa
    <*> pb

parse_line :: P.ReadP (ParserState ())
parse_line = sequence_ <$> P.manyTill
    (P.choice
        [ consume_dots  <$> P.munch1  (== '.')
        , consume_numb  <$> P.munch1  C.isDigit
        , consume_symb  <$> P.satisfy (\c -> c /= '.' && not (C.isDigit c))
        ])
    P.eof

known_numbers :: M.Map (Int, Int) Char -> M.Map (Int, Int) Int -> M.Map Int Int -> [Int]
known_numbers symbols locations values = fmap (values M.!) . S.toList $ known_unq (M.toList symbols)  where
    known_unq []     = S.empty
    known_unq (s:ss) = let
        acc'   = known_unq ss
        ((y,x), v) = s
        keys  =
            [(y-1,x-1),(y-1,x),(y-1,x+1)
            ,(y  ,x-1),        (y  ,x+1)
            ,(y+1,x-1),(y+1,x),(y+1,x+1)
            ]
        exists = mapMaybe (locations M.!?) keys

        in  foldl' (flip S.insert) acc' exists

gear_ratio :: M.Map (Int, Int) Char -> M.Map (Int, Int) Int -> M.Map Int Int -> Int
gear_ratio symbols locations values = foldl' go 0 (M.toList symbols) where
    go  acc ((y,x), v)  = if v /= '*' then acc else let
        keys =
            [(y-1,x-1),(y-1,x),(y-1,x+1)
            ,(y  ,x-1),        (y  ,x+1)
            ,(y+1,x-1),(y+1,x),(y+1,x+1)
            ]
        exists = S.toList . S.fromList $ mapMaybe (locations M.!?) keys
        in if length exists /= 2 then acc else let
                r = product $ map (values M.!) exists
            in  r + acc


main_1 :: IO ()
main_1 = do
    input <- lines <$> readFile "./inputs/day-3.input"
    let state_action = forM input (forceParse ((*>) <$> parse_line <*> pure consume_nl))
    let PE{symb = fsy, vals = fva, seen = fse } = S.execState state_action initial_env

    print $ sum $ known_numbers fsy fva fse

main_2 :: IO ()
main_2 = do
    input <- lines <$> readFile "./inputs/day-3.input"
    let state_action = forM input (forceParse ((*>) <$> parse_line <*> pure consume_nl))
    let PE{symb = fsy, vals = fva, seen = fse } = S.execState state_action initial_env

    --print fsy
    --print fva 
    --print fse
    print $ gear_ratio fsy fva fse