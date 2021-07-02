module Syntax.Values

import Syntax.Lexer 
import Data.SortedMap
import Data.List

public export
record Pos where
    constructor MkPos
    line, column : Int

public export
data TomlError 
    = LexicalErr Pos 
    | ParsingErr String Pos
    | EofErr 
    
public export
Show TomlError where 
    show (LexicalErr pos) = 
        "LexicalErr on line " ++ (show pos.line) ++ " and column " ++ (show pos.column)
    show (ParsingErr str pos) = 
        str ++ " on line " ++ (show pos.line) ++ " and column " ++ (show pos.column)
    show EofErr =
        "End of file"
mutual 
    public export
    data Value = 
        VStr String
        | VInt        Integer 
        | VFloat      Double 
        | VBool       Bool 
        | VDatetime   Integer
        | VArray      (List Value)  
        | VTable      Toml

    public export
    Toml : Type
    Toml = (SortedMap String Value)