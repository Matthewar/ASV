module Parser.TokenTypes where

import Parser.PositionWrapper

import Data.Char as Char
import Data.Int (Int64)
import Data.ByteString.Char8 as ByteString

-- |Token Type
data Token =
   -- |Any reserved word
   -- Used to identify statements
   Keyword ReservedWord
   -- |Operators
   -- Used to identify statements and in expressions
   | Operator OperatorType
   -- |Identifier
   -- Names and markers
   | Identifier String
   -- |Literals
   -- Values
   | Literal LitType
   -- |End of File marker
   | EOF
   deriving (Eq,Show)

-- |Token with position information
type WrappedToken = PosnWrapper Token

-- |All keywords
data ReservedWord = Abs
                  | Access
                  | After
                  | Alias
                  | All
                  | And
                  | Architecture
                  | Array
                  | Assert
                  | Attribute
                  | Begin
                  | Block
                  | Body
                  | Buffer
                  | Bus
                  | Case
                  | Component
                  | Configuration
                  | Constant
                  | Disconnect
                  | Downto
                  | Else
                  | Elsif
                  | End
                  | Entity
                  | Exit
                  | File
                  | For
                  | Function
                  | Generate
                  | Generic
                  | Guarded
                  | If
                  | In
                  | Inout
                  | Is
                  | Label
                  | Library
                  | Linkage
                  | Loop
                  | Map
                  | Mod
                  | Nand
                  | New
                  | Next
                  | Nor
                  | Not
                  | Null
                  | Of
                  | On
                  | Open
                  | Or
                  | Others
                  | Out
                  | Package
                  | Port
                  | Procedure
                  | Process
                  | Range
                  | Record
                  | Register
                  | Rem
                  | Report
                  | Return
                  | Select
                  | Severity
                  | Signal
                  | Subtype
                  | Then
                  | To
                  | Transport
                  | Type
                  | Units
                  | Until
                  | Use
                  | Variable
                  | Wait
                  | When
                  | While
                  | With
                  | Xor
                  deriving (Eq,Show)

-- |Base of bit string literals
data LiteralBase =
   -- |Binary base
   -- Marked by B character
   BinBased
   -- |Octal base
   -- Marked by O character
   | OctBased
   -- |Hexadecimal base
   -- Marked by X character
   | HexBased
   deriving (Eq,Show,Ord)

-- |Literal token types
data LitType =
   -- |Universal Integer
   -- Uses 'Int64' so bounded by 64 bits
   Univ_Int Int64
   -- |Universal Real
   -- Uses 'Double' so bounded by implementation of this
   | Univ_Real Double
   -- |Bit string literal
   | BitStr LiteralBase ByteString
   -- |String literal
   | Str String
   -- |Character literal
   | Character Char
   deriving (Eq,Show)

-- |Operator token types
data OperatorType = Arrow              -- ^ =>
                  | DoubleStar         -- ^ \\**
                  | VarAssign          -- ^ :=
                  | Inequality         -- ^ /=
                  | GreaterThanOrEqual -- ^ >=
                  | SignAssign         -- ^ <=
                  | Box                -- ^ <>
                  | Ampersand          -- ^ &
                  | Apostrophe         -- ^ '
                  | LeftParen          -- ^ (
                  | RightParen         -- ^ )
                  | Star               -- ^ \\*
                  | Plus               -- ^ +
                  | Comma              -- ^ ,
                  | Hyphen             -- ^ -
                  | Period             -- ^ .
                  | Slash              -- ^ /
                  | Colon              -- ^ :
                  | Semicolon          -- ^ ;
                  | LessThan           -- ^ <
                  | Equal              -- ^ =
                  | GreaterThan        -- ^ >
                  | Bar                -- ^ \\|
                  deriving (Eq)

instance Show OperatorType where
   show Arrow              = "=>"
   show DoubleStar         = "**"
   show VarAssign          = ":="
   show Inequality         = "/="
   show GreaterThanOrEqual = ">="
   show SignAssign         = "<="
   show Box                = "<>"
   show Ampersand          = "&"
   show Apostrophe         = "'"
   show LeftParen          = "("
   show RightParen         = ")"
   show Star               = "*"
   show Plus               = "+"
   show Comma              = ","
   show Hyphen             = "-"
   show Period             = "."
   show Slash              = "/"
   show Colon              = ":"
   show Semicolon          = ";"
   show LessThan           = "<"
   show Equal              = "="
   show GreaterThan        = ">"
   show Bar                = "|"
