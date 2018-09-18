{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           Data.Functor.Identity
import           Data.List.Split
import           Data.Maybe
import           Text.Parsec
import           Text.Read             (readMaybe)

type Sql = String

data Token
  = Literal String
  | Parameter
  deriving Show

data Placeholder
  = Placeholder {
      placeholderType :: ParameterType
    , placeholderText :: String
  }
  deriving Show

data ParameterType
  = Number
  | Text
  | Date
  deriving Show

buildSql :: String -> Either ParseError Sql
buildSql s =
  let ls      = lines s
      sqlLine = extractSql (ls!!0)
      params  = map toPlaceholder $ splitOn ", " $ omitBracket (ls!!1)
  in
    case parseParametarizeSql sqlLine of
      Left err -> Left err
      Right ts -> Right $ f ts params ""
  where
    f :: [Token] -> [Placeholder] -> String -> String
    f [] _ accum                = accum
    f ((Literal s):ts) ps     accum = f ts ps (accum ++ s)
    f (Parameter:ts)   (p:ps) accum = f ts ps (accum ++ (placeHolderToString p))
    placeHolderToString (Placeholder Number s) = s
    placeHolderToString (Placeholder Text   s) = "'" ++ s ++ "'"
    placeHolderToString (Placeholder Date   s) = "'" ++ s ++ "'"

extractSql :: String -> String
extractSql s = (splitOn "--" s) !! 3

omitBracket :: String -> String
omitBracket s = reverse $ tail $ dropWhile (\c -> c /= ']') $ reverse $ tail $ dropWhile (\c -> c /= '[') s

toPlaceholder :: String -> Placeholder
toPlaceholder s =
  case isNumber s of
    Just p  -> p
    Nothing ->
      case isDate s of
        Just p  -> p
        Nothing -> Placeholder Text s

isNumber :: String -> Maybe Placeholder
isNumber s =
  case readMaybe s :: Maybe Integer of
    Nothing -> Nothing
    Just _  -> Just $ Placeholder Number s

isDate :: String -> Maybe Placeholder
isDate s =
  let ts = splitOn "-" s
  in  if length ts == 3
    then
      case readMaybe (ts!!0) :: Maybe Integer of
        Nothing -> Nothing
        Just _  -> Just $ Placeholder Date s
    else Nothing

parseParametarizeSql :: String -> Either ParseError [Token]
parseParametarizeSql sql = parse parametarizedSqlParser "" sql

parametarizedSqlParser :: ParsecT String u Identity [Token]
parametarizedSqlParser = do
  first::String  <- many $ noneOf "?"
  remain::[Token] <- remainingSqlParser <|> return []
  return $ (Literal first):remain

remainingSqlParser :: ParsecT String u Identity [Token]
remainingSqlParser = do
  _  <- char '?'
  ts::[Token] <- parametarizedSqlParser
  return $ Parameter:ts
