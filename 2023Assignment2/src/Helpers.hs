module Helpers where

import Instances
import Parser
import GHC.Base
import Data.String (String)
import Data.List (init)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = p <:> many (sep *> p)
  where
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = liftA2 (:)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

replaceLast :: String -> String -> String
replaceLast [] _ = []
replaceLast str replace = init str ++ replace