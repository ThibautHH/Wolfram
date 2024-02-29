module Main (main) where

import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

import Conf (getConf, Conf(Conf, rule))
import Lib (wolfram)

main :: IO ()
main = do
    conf <- getConf
    case conf of
        Nothing -> hPutStrLn stderr "Invalid arguments" >> exitWith (ExitFailure 84)
        Just Conf {rule=Nothing} -> hPutStrLn stderr "No rule specified, use `--rule <rule number>'" >> exitWith (ExitFailure 84)
        Just config -> wolfram config
    exitSuccess
