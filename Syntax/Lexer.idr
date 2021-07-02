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
eqTknTag : TokenKind -> TokenKind -> Bool
eqTknTag (StringLit _) (StringLit _) = True
eqTknTag (DateLit _) (DateLit _) = True
eqTknTag (NumberLit _) (NumberLit _) = True
eqTknTag (FloatLit _) (FloatLit _) = True
eqTknTag (Identifier _) (Identifier _) = True
eqTknTag FalseLit FalseLit = True
eqTknTag TrueLit TrueLit = True
eqTknTag (Comment _) (Comment _) = True
eqTknTag Eq Eq = True
eqTknTag Comma Comma = True
eqTknTag Dot Dot = True
eqTknTag LCurly LCurly = True
eqTknTag RCurly RCurly = True
eqTknTag LBracket LBracket = True
eqTknTag RBracket RBracket = True
eqTknTag a b = False

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

public export
keyword : Lexer
keyword = some (pred (\x => isAlphaNum x || x == '-' || x == '_'))

public export
string : Lexer
string = (is '"') <+> some (pred (/= '"')) <+> (is '"')

public export
comment : Lexer
comment = (is '#') <+> some (pred (/= '\n')) <+> ((is '\n') <|> empty)

public export
token_map : TokenMap TokenKind
token_map =
   [(digits <+> (is '.') <+> digits, \x => FloatLit (cast x)), 
   (digits  , NumberLit . cast),
   (is '['  , const LBracket),
   (is ']'  , const  RBracket),
   (is '{'  , const  LCurly),
   (is '}'  , const  RCurly),
   (space   , const Whitespace),
   (is '='  , const  Eq),
   (is '.'  , const Dot),
   (is ','  , const Comma),
   (comment , Comment),
   (string  , StringLit),
   (is '\n' <|> is '\r', const Whitespace),
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
         (tokens, _, _, "") => Right(filter removeUseless tokens)
         (_, line, column, _) => Left (line, column)

