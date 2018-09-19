{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.PrettyBySqlformtOrg where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           GHC.Generics
import           Network.HTTP.Simple

data Res = Res{ result :: String }
  deriving (Generic, Show)
instance FromJSON Res

pretty :: String -> IO String
pretty sql = do
  let req = setRequestBodyURLEncoded [("reindent", "1"), ("sql", B.pack sql)]
          $ setRequestMethod "POST" "https://sqlformat.org/api/v1/format"
  res::Res <- httpJSON req >>= return . getResponseBody
  return $ result res
