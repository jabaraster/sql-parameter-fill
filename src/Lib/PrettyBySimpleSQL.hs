module Lib.PrettyBySimpleSQL where

import           Language.SQL.SimpleSQL.Parser
import           Language.SQL.SimpleSQL.Pretty
import           Language.SQL.SimpleSQL.Syntax


pretty :: String -> Either ParseError String
pretty s =
  case parseQueryExpr SQL2011 "" Nothing s of
    Left  err  -> Left err
    Right expr -> Right $ prettyQueryExpr SQL2011 expr

