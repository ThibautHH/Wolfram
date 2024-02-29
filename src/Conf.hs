module Conf (Conf(Conf, rule, start, width, iterations, offset), getConf) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Rule (Rule, maybeRule)

data Conf = Conf {
    rule :: Maybe Rule,
    start :: Int,
    iterations :: Maybe Int,
    width :: Int,
    offset :: Int
} deriving Show

maybePositive :: Int -> Maybe Int
maybePositive x | x > 0 = Just x
                | otherwise = Nothing

readConf :: Conf -> [String] -> Maybe Conf
readConf c [] = Just c
readConf c ("--rule":r:xs) = (\x -> readConf c {rule = Just x} xs) =<< maybeRule =<< readMaybe r
readConf c ("--start":s:xs) = (\x -> readConf c {start = x} xs) =<< maybePositive =<< readMaybe s
readConf c ("--lines":i:xs) = (\x -> readConf c {iterations = Just x} xs) =<< maybePositive =<< readMaybe i
readConf c ("--window":w:xs) = (\x -> readConf c {width = x} xs) =<< maybePositive =<< readMaybe w
readConf c ("--move":m:xs) = (\x -> readConf c {offset = x} xs) =<< readMaybe m
readConf _ _ = Nothing

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing,
    start = 0,
    iterations = Nothing,
    width = 80,
    offset = 0
}

getConf :: IO (Maybe Conf)
getConf = readConf defaultConf <$> getArgs
