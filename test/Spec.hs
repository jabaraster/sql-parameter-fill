import           Language.SQL.SimpleSQL.Parser
import           Language.SQL.SimpleSQL.Pretty
import           Language.SQL.SimpleSQL.Syntax

main :: IO ()
main = do
  case parseQueryExpr SQL2011 "" Nothing "select * from hoge where id = ?" of
    Left err   -> print err
    Right expr -> putStrLn $ prettyQueryExpr SQL2011 expr

prettySql :: String -> String
prettySql s =
  case parseQueryExpr SQL2011 "" Nothing s of
    Left  err  -> show err
    Right expr -> prettyQueryExpr SQL2011 expr
