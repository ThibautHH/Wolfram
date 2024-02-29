{-# LANGUAGE NamedFieldPuns #-}

module Lib (Line, showLine, wolfram) where

import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NE

import Conf (Conf(Conf, width, offset))
import Rule(Rule(r111, r110, r101, r100, r011, r010, r001, r000))

data Cell = Alive | Dead

instance Show Cell where
    show Alive = "*"
    show Dead = " "

data Line = Line {
    left :: [Cell],
    middle :: NonEmpty Cell,
    right :: [Cell]
}

newLine :: NonEmpty Cell -> Line
newLine ls = Line (repeat Dead) ls (repeat Dead)

defaultLine :: Line
defaultLine = newLine $ NE.singleton Alive

leftWidth :: Conf -> Int
leftWidth Conf{width, offset} = (width `div` 2) + offset

rightWidth :: Conf -> Int
rightWidth Conf{width, offset} | even width = (width `div` 2) - 1 - offset
                               | otherwise = (width `div` 2) - offset

showLine :: Conf -> Line -> String
showLine conf Line{left, middle, right} | rightWidth conf < 0 = join $ map show $ reverse $ take (leftWidth conf) left
                                        | leftWidth conf < 0 = join $ map show $ take (rightWidth conf) right
                                        | otherwise = join $ map show $ join [reverse $ take (leftWidth conf) left, toList middle, take (rightWidth conf) right]

ruleToCell :: Bool -> Cell
ruleToCell False = Dead
ruleToCell True = Alive

nextCell :: (Cell, Cell, Cell) -> Rule -> Cell
nextCell (Alive, Alive, Alive) = ruleToCell . r111
nextCell (Alive, Alive, Dead) = ruleToCell . r110
nextCell (Alive, Dead, Alive) = ruleToCell . r101
nextCell (Alive, Dead, Dead) = ruleToCell . r100
nextCell (Dead, Alive, Alive) = ruleToCell . r011
nextCell (Dead, Alive, Dead) = ruleToCell . r010
nextCell (Dead, Dead, Alive) = ruleToCell . r001
nextCell (Dead, Dead, Dead) = ruleToCell . r000

nextFromList :: [Cell] -> Rule -> Cell
nextFromList (x:y:z:_) = nextCell (x, y, z)
nextFromList _ = const Dead

wolfram :: Conf -> IO [Line]
wolfram = const $ pure [defaultLine]
