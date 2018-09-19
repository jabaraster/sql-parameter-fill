module Lib.PrettyByHsSqlPpp where

import           Data.Text.Lazy            (dropWhile, pack, reverse, tail, unpack)
import           Database.HsSqlPpp.Dialect
import           Database.HsSqlPpp.Parse
import           Database.HsSqlPpp.Pretty
import           Prelude                   hiding (dropWhile, reverse, tail)

pretty :: String -> Either ParseErrorExtra String
pretty s =
  case parseStatements defaultParseFlags "" Nothing (pack (s++";")) of -- 末尾に;を付けないといけないのは非常にいけてない・・・
    Left  err    -> Left err
    Right parsed -> Right $ unpack
                    $ reverse
                    $ tail
                    $ dropWhile (\c -> c /= ';')
                    $ reverse
                    $ prettyStatements defaultPrettyFlags parsed
