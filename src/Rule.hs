{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Rule
-}

module Rule (Rule(Rule, r111, r110, r101, r100, r011, r010, r001, r000), maybeRule) where

import Data.Bits ((.&.), complement)

data Rule = Rule {
    r111 :: Bool,
    r110 :: Bool,
    r101 :: Bool,
    r100 :: Bool,
    r011 :: Bool,
    r010 :: Bool,
    r001 :: Bool,
    r000 :: Bool
} deriving Show

maybeRule :: Int -> Maybe Rule
maybeRule x | (x .&. complement 0xff) /= 0 = Nothing
            | otherwise = Just (Rule
                ((x .&. 0x80) /= 0)
                ((x .&. 0x40) /= 0)
                ((x .&. 0x20) /= 0)
                ((x .&. 0x10) /= 0)
                ((x .&. 0x08) /= 0)
                ((x .&. 0x04) /= 0)
                ((x .&. 0x02) /= 0)
                ((x .&. 0x01) /= 0))
