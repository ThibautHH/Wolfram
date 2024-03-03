{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

import Conf (getConf, Conf(Conf, rule))
import Lib (wolfram)

missingRule :: String
missingRule = "No rule specified, use `--rule <rule number>'"

main :: IO ()
main = do
    conf <- getConf
    _ <- case conf of
        Nothing -> hPutStrLn stderr "Invalid arguments"
            >> exitWith (ExitFailure 84)
        Just Conf {rule=Nothing} -> hPutStrLn stderr missingRule
            >> exitWith (ExitFailure 84)
        Just config -> wolfram config
    exitSuccess
