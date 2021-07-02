module Syntax.Lexer

import Text.Lexer
import Data.List

public export
data TokenKind
    = StringLit String
    | DateLit Integer
    | NumberLit Integer
    | FloatLit Double
    | Identifier String 
    | Comment String
    | TrueLit
    | FalseLit
    | LBracket
    | RBracket
    | LCurly
    | RCurly
    | Dot
    | Comma
    | Eq 
    | Whitespace
    | EOF

-- Not implemented the `==` cause they're not equal. 
-- They just have the same tkn

public export 
eq_tkn : TokenKind -> TokenKind -> Bool
eq_tkn (StringLit _) (StringLit _) = True
eq_tkn (DateLit _) (DateLit _) = True
eq_tkn (NumberLit _) (NumberLit _) = True
eq_tkn (FloatLit _) (FloatLit _) = True
eq_tkn (Identifier _) (Identifier _) = True
eq_tkn FalseLit FalseLit = True
eq_tkn TrueLit TrueLit = True
eq_tkn (Comment _) (Comment _) = True
eq_tkn Eq Eq = True
eq_tkn Comma Comma = True
eq_tkn Dot Dot = True
eq_tkn LCurly LCurly = True
eq_tkn RCurly RCurly = True
eq_tkn LBracket LBracket = True
eq_tkn RBracket RBracket = True
eq_tkn a b = False

public export 
Show TokenKind where 
    show (StringLit x) = x
    show (DateLit x) = "Date"
    show (NumberLit x) = cast x
    show (FloatLit x) = cast x 
    show (Identifier x) = x
    show (FalseLit) = "False"
    show (TrueLit) = "True"
    show (Comment x) = "#"
    show Eq = "="
    show Comma = ","
    show Dot = "."
    show LCurly = "{"
    show RCurly = "}"
    show LBracket = "["
    show RBracket = "]"
    show Whitespace = " "
    show EOF = "EOF"

export
opChars : String
opChars = "+-*"

operator : Lexer
operator = some (oneOf opChars)

toInt' : String -> Integer
toInt' = cast

public export
keyword : Lexer
keyword = some (pred (\x => isAlphaNum x || x == '-' || x == '_'))

public export
string : Lexer
string = (is '"') <+> some (pred (\x => x /= '"')) <+> (is '"')

public export
comment : Lexer
comment = (is '#') <+> some (pred (\x => x /= '\n')) <+> ((is '\n') <|> empty)

public export
token_map : TokenMap TokenKind
token_map =
   [(digits <+> (is '.') <+> digits, \x => FloatLit (cast x)), 
   (digits, \x => NumberLit (cast x)),
   (is '[' ,\x => LBracket),
   (is ']' ,\x => RBracket),
   (is '{' ,\x => LCurly),
   (is '}' ,\x => RCurly),
   (comment ,\x => Comment x),
   (is '=' ,\x => Eq),
   (is '.' ,\n => Dot),
   (is ',' ,\n => Comma),
   (string ,\x => StringLit x),
   (space  ,\x => Whitespace),
   (is '\n' <|> is '\r', \x => Whitespace),
   (keyword, \x => case x of 
            "true" => TrueLit
            "false" => FalseLit
            x => Identifier x)]

public export
removeUseless : TokenData TokenKind -> Bool
removeUseless kind = 
     case (TokenData.tok kind) of 
          Whitespace => False
          EOF => False 
          Comment _ => False
          _ => True

public export
lexToml : String -> Either (Int, Int) (List (TokenData TokenKind))
lexToml str
  = case lex token_map str of
         (tokens, _, _, "") => Right 
            $ (filter removeUseless tokens)
         (_, line, column, _) => Left $ (line, column)

