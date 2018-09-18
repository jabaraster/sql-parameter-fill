module Main where

import Lib
import System.Exit

main :: IO ()
main = do
  cs <- getContents
  case buildSql cs of
    Left err -> do
      putStrLn $ show err
      exitWith $ ExitFailure 1
    Right s  -> putStrLn s
