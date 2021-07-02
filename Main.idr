module Main

import Data.SortedMap
import Text.Parser
import Syntax.Values 
import Data.List
import Syntax.Lexer
import Syntax.Grammar

public export 
deserialize : String -> Either String Toml
deserialize code = 
    case parseToml code of 
        Left x => Left "Oh no!"
        Right toml => Right toml

public export
lookup : String -> Toml -> (Maybe Value)
lookup key toml = SortedMap.lookup key toml 