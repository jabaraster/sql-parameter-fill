{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           GHC.Generics
import           Lib
import           Network.HTTP.Simple
import           System.Exit

main :: IO ()
main = do
  cs <- getContents
  case buildSql cs of
    Left err -> do
      putStrLn $ show err
      exitWith $ ExitFailure 1
    Right s  -> pretty s >>= putStrLn

data Res = Res{ result :: String }
  deriving (Generic, Show)
instance FromJSON Res

pretty :: Sql -> IO Sql
pretty sql = do
  let req = setRequestBodyURLEncoded [("reindent", "1"), ("sql", B.pack sql)]
          $ setRequestMethod "POST" "https://sqlformat.org/api/v1/format"
  res::Res <- httpJSON req >>= return . getResponseBody
  return $ result res
