module Main where

import           Prelude                 hiding (fail)
import           System.Exit

import           Lib
import qualified Lib.PrettyByHsSqlPpp    as PH
import qualified Lib.PrettyBySimpleSQL   as PS
import qualified Lib.PrettyBySqlformtOrg as PQ

main :: IO ()
main = do
  cs <- getContents
  case buildSql cs of
    Left err -> fail err
    Right s  -> case PH.pretty s of
      Left err -> fail err
      Right s' -> putStrLn s'

fail err = do
  putStrLn $ show err
  exitWith $ ExitFailure 1
