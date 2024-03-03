{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main library
-}

{-# LANGUAGE NamedFieldPuns #-}

module Lib (Line, showLine, wolfram) where

import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import qualified Data.List.NonEmpty as NE

import Conf (Conf(Conf, rule, width, offset, start, iterations))
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

nextSide :: Bool -> [Cell] -> [Cell] -> Int -> Rule -> [Cell]
nextSide _ _ next (-1) _ = next
nextSide False prev next gen rule = nextSide False prev (nextFromList (reverse . take 3 $ drop gen prev) rule:next) (gen - 1) rule
nextSide True prev next gen rule = nextSide True prev (nextFromList (drop gen prev) rule:next) (gen - 1) rule

nextMiddle :: NonEmpty Cell -> [Cell] -> Rule -> NonEmpty Cell
nextMiddle ls@(_:|y:z:xs) next rule = nextMiddle (y:|z:xs) (nextFromList (toList ls) rule:next) rule
nextMiddle _ next _ = fromList next

iterateLine :: Line -> Int -> Rule -> Line
iterateLine Line{left,middle,right} gen rule = Line
    (nextSide False (NE.head middle:left) (repeat Dead) gen rule)
    (nextMiddle (head left:|(toList middle ++ [head right])) [] rule)
    (nextSide True (NE.last middle:right) (repeat Dead) gen rule)

makeLines :: Conf -> Int -> NonEmpty Line -> NonEmpty Line
makeLines Conf{rule=Nothing} _ ls = ls
makeLines Conf{iterations=Nothing} _ ls = ls
makeLines Conf{iterations=Just 1} _ ls = ls
makeLines conf@(Conf{rule=Just rule, start=0, iterations=Just i}) gen ls@(x:|_) = makeLines conf{iterations=Just (i - 1)} (gen + 1)
    $ iterateLine x gen rule:|toList ls
makeLines conf@(Conf{rule=Just rule, start}) gen (x:|_) = makeLines conf{start=start - 1} (gen + 1)
    $ NE.singleton $ iterateLine x gen rule

makePrintLines :: Conf -> Int -> IO (NonEmpty Line) -> IO (NonEmpty Line)
makePrintLines Conf{rule=Nothing} _ ls = ls
makePrintLines conf@(Conf{rule=Just rule, start=0}) gen ls = do
    stack <- ls
    (putStrLn . showLine conf) $ NE.head stack
    makePrintLines conf (gen + 1) $ pure $ iterateLine (NE.head stack) gen rule:|toList stack
makePrintLines conf@(Conf{rule=Just rule, start}) gen ls = do
    stack <- ls
    makePrintLines conf{start=start - 1} (gen + 1) $ pure
        $ NE.singleton $ iterateLine (NE.head stack) gen rule

wolfram :: Conf -> IO ()
wolfram conf@(Conf{iterations=Nothing}) = do
    _ <- makePrintLines conf 0 $ pure $ NE.singleton defaultLine
    return ()
wolfram conf@(Conf{iterations=Just _}) = mapM_ (putStrLn . showLine conf) $ NE.reverse
    $ makeLines conf 0 $ NE.singleton defaultLine
