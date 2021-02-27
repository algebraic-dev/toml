module Grammar

import Lexer
import Values
import Text.Parser
import Text.Lexer.Core
import Data.List
import Data.String.Extra
import Data.SortedMap

public export 
insertPath : Toml -> List (String) -> Value -> Toml 
insertPath toml path value = 
     case path of
     hd::tl => 
          (case (lookup hd toml) of 
          Just (VTable oToml) => 
               let oToml = insertPath oToml tl value in 
               insert hd (VTable oToml) toml  
          _ => 
               insert (join "." path) value toml)
     _ => empty  

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData TokenKind) True ty

term : TokenKind -> Rule Bool
term y = terminal ("Expected Symbol")
     (\x => if eq_tkn (TokenData.tok x) y then 
               Just True
            else 
               Nothing)

term_ret :  (TokenData TokenKind -> Maybe x) -> Rule x
term_ret fn = terminal "Unexpected Pattern" fn

-- Some useful terminal functions

bool : Rule Value
bool = (term_ret (\n => 
          case TokenData.tok n of
               TrueLit => Just (VBool True)
               FalseLit => Just (VBool False)
               _ => Nothing))

int : Rule Value
int = (term_ret (\n => 
          case TokenData.tok n of
               NumberLit x => Just (VInt x)
               _ => Nothing))

str : Rule Value
str = (term_ret (\n => 
          case TokenData.tok n of
               StringLit x => Just (VStr x)
               _ => Nothing))

float : Rule Value
float = (term_ret (\n => 
          case TokenData.tok n of
               FloatLit x => Just (VFloat x)
               _ => Nothing))

identifier : Rule String
identifier = (term_ret (\n => 
          case (TokenData.tok n) of
               Identifier x => Just (x)
               _ => Nothing))

-- These functions matches brackets and with it
-- We can make a type checking on the array

brackets : Rule Value -> Rule Value 
brackets exp = do 
     (term LBracket)
     r <- sepBy1 (term Comma) exp
     (term RBracket)
     pure (VArray r)


empty_array : Rule Value 
empty_array = do 
     (term LBracket)
     (term RBracket)
     pure (VArray [])

createArrayTypes : List (Rule Value) -> Rule Value 
createArrayTypes exps =  
     foldl (\acc => \cur => (<|>) acc (brackets cur)) empty_array exps

array' : Rule Value 
array' = createArrayTypes [int, str, float, bool] 

array : Rule Value 
array = array'

-- General rules 

values : Rule Value 
values =  int 
      <|> bool
      <|> str 
      <|> float
      <|> array

keyValue : Rule (List String, Value)
keyValue = do 
     name <- identifier 
     (term Eq)
     res <- values
     pure ([name], res)

-- I'm certain that least one token it will have.
-- Probably i'll change it to a Vect in the future

fst : List String -> String
fst name = 
     case name of 
          hd::[] => hd
          _ => "????"


path : Rule (List String)
path = sepBy1 (term Dot) (identifier)

table : Rule ((List String), Value)
table = do 
     (term LBracket)
     name <- path 
     (term RBracket)
     pair <- many keyValue
     pure (
          name, 
          VTable (foldl (\acc => \(key, val) => insert (fst key) val acc) empty pair))

toml : Rule Toml 
toml =  do
     pair <- some (table <|> keyValue)
     pure $ foldl (\acc => 
          \(path, value) => 
               insertPath acc path value) empty pair

public export
parseToml : String -> Either
                         (TomlError)
                         (Toml)

parseToml str = 
     let res = (lexToml str) in
     case res of 
       Left (line, column) => 
          Left (LexicalErr (MkPos line column))
       Right list => 
          (case parse (toml) list of 
               Left (Error x y) => 
                    Left (case y of 
                         hd::_ => 
                              let pos = (MkPos 
                                             (TokenData.line hd) 
                                             (TokenData.col hd)) in
                              (ParsingErr x pos)
                         _     => EofErr)
               Right (value, _) => Right value)