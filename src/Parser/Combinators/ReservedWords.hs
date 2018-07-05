{-|
   Module      : Parser.Combinators.ReservedWords
   Description : Combinators for the "reserved word" lexical elements of VHDL.
|-}
module Parser.Combinators.ReservedWords
   ( absKeyword
   , accessKeyword
   , afterKeyword
   , aliasKeyword
   , allKeyword
   , andKeyword
   , architectureKeyword
   , arrayKeyword
   , assertKeyword
   , attributeKeyword
   , beginKeyword
   , blockKeyword
   , bodyKeyword
   , bufferKeyword
   , busKeyword
   , caseKeyword
   , componentKeyword
   , configurationKeyword
   , constantKeyword
   , disconnectKeyword
   , downtoKeyword
   , elseKeyword
   , elsifKeyword
   , endKeyword
   , entityKeyword
   , exitKeyword
   , fileKeyword
   , forKeyword
   , functionKeyword
   , generateKeyword
   , genericKeyword
   , guardedKeyword
   , ifKeyword
   , inKeyword
   , inoutKeyword
   , isKeyword
   , labelKeyword
   , libraryKeyword
   , linkageKeyword
   , loopKeyword
   , mapKeyword
   , modKeyword
   , nandKeyword
   , newKeyword
   , nextKeyword
   , norKeyword
   , notKeyword
   , nullKeyword
   , ofKeyword
   , onKeyword
   , openKeyword
   , orKeyword
   , othersKeyword
   , outKeyword
   , packageKeyword
   , portKeyword
   , procedureKeyword
   , processKeyword
   , rangeKeyword
   , recordKeyword
   , registerKeyword
   , remKeyword
   , reportKeyword
   , returnKeyword
   , selectKeyword
   , severityKeyword
   , signalKeyword
   , subtypeKeyword
   , thenKeyword
   , toKeyword
   , transportKeyword
   , typeKeyword
   , unitsKeyword
   , untilKeyword
   , useKeyword
   , variableKeyword
   , waitKeyword
   , whenKeyword
   , whileKeyword
   , withKeyword
   , xorKeyword
   ) where

import Control.Applicative
import Data.Char
         ( toUpper
         , toLower
         )
import Text.Parsec (notFollowedBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
         ( char
         , oneOf
         )

-- |Parse a character, ignoring case
-- Parses 'Char' input (upper or lower case)
-- Returns upper case character parsed
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar chr =
   char (toUpper chr)
   <|> (char (toLower chr) *> return (toUpper chr))

-- |Parses a keyword, ignoring case
-- Parses 'String' input (upper or lower case)
-- Returns '()'
keywordParser :: String -> Parser ()
keywordParser keyword = mapM caseInsensitiveChar keyword *> (notFollowedBy $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

-- |Parses the keyword "abs", ignoring case
-- Returns '()'
absKeyword :: Parser ()
absKeyword = keywordParser "abs"

-- |Parses the keyword "access", ignoring case
-- Returns '()'
accessKeyword :: Parser ()
accessKeyword = keywordParser "access"

-- |Parses the keyword "after", ignoring case
-- Returns '()'
afterKeyword :: Parser ()
afterKeyword = keywordParser "after"

-- |Parses the keyword "alias", ignoring case
-- Returns '()'
aliasKeyword :: Parser ()
aliasKeyword = keywordParser "alias"

-- |Parses the keyword "all", ignoring case
-- Returns '()'
allKeyword :: Parser ()
allKeyword = keywordParser "all"

-- |Parses the keyword "and", ignoring case
-- Returns '()'
andKeyword :: Parser ()
andKeyword = keywordParser "and"

-- |Parses the keyword "architecture", ignoring case
-- Returns '()'
architectureKeyword :: Parser ()
architectureKeyword = keywordParser "architecture"

-- |Parses the keyword "array", ignoring case
-- Returns '()'
arrayKeyword :: Parser ()
arrayKeyword = keywordParser "array"

-- |Parses the keyword "assert", ignoring case
-- Returns '()'
assertKeyword :: Parser ()
assertKeyword = keywordParser "assert"

-- |Parses the keyword "attribute", ignoring case
-- Returns '()'
attributeKeyword :: Parser ()
attributeKeyword = keywordParser "attribute"

-- |Parses the keyword "begin", ignoring case
-- Returns '()'
beginKeyword :: Parser ()
beginKeyword = keywordParser "begin"

-- |Parses the keyword "block", ignoring case
-- Returns '()'
blockKeyword :: Parser ()
blockKeyword = keywordParser "block"

-- |Parses the keyword "body", ignoring case
-- Returns '()'
bodyKeyword :: Parser ()
bodyKeyword = keywordParser "body"

-- |Parses the keyword "buffer", ignoring case
-- Returns '()'
bufferKeyword :: Parser ()
bufferKeyword = keywordParser "buffer"

-- |Parses the keyword "bus", ignoring case
-- Returns '()'
busKeyword :: Parser ()
busKeyword = keywordParser "bus"

-- |Parses the keyword "case", ignoring case
-- Returns '()'
caseKeyword :: Parser ()
caseKeyword = keywordParser "case"

-- |Parses the keyword "component", ignoring case
-- Returns '()'
componentKeyword :: Parser ()
componentKeyword = keywordParser "component"

-- |Parses the keyword "configuration", ignoring case
-- Returns '()'
configurationKeyword :: Parser ()
configurationKeyword = keywordParser "configuration"

-- |Parses the keyword "constant", ignoring case
-- Returns '()'
constantKeyword :: Parser ()
constantKeyword = keywordParser "constant"

-- |Parses the keyword "disconnect", ignoring case
-- Returns '()'
disconnectKeyword :: Parser ()
disconnectKeyword = keywordParser "disconnect"

-- |Parses the keyword "downto", ignoring case
-- Returns '()'
downtoKeyword :: Parser ()
downtoKeyword = keywordParser "downto"

-- |Parses the keyword "else", ignoring case
-- Returns '()'
elseKeyword :: Parser ()
elseKeyword = keywordParser "else"

-- |Parses the keyword "elsif", ignoring case
-- Returns '()'
elsifKeyword :: Parser ()
elsifKeyword = keywordParser "elsif"

-- |Parses the keyword "end", ignoring case
-- Returns '()'
endKeyword :: Parser ()
endKeyword = keywordParser "end"

-- |Parses the keyword "entity", ignoring case
-- Returns '()'
entityKeyword :: Parser ()
entityKeyword = keywordParser "entity"

-- |Parses the keyword "exit", ignoring case
-- Returns '()'
exitKeyword :: Parser ()
exitKeyword = keywordParser "exit"

-- |Parses the keyword "file", ignoring case
-- Returns '()'
fileKeyword :: Parser ()
fileKeyword = keywordParser "file"

-- |Parses the keyword "for", ignoring case
-- Returns '()'
forKeyword :: Parser ()
forKeyword = keywordParser "for"

-- |Parses the keyword "function", ignoring case
-- Returns '()'
functionKeyword :: Parser ()
functionKeyword = keywordParser "function"

-- |Parses the keyword "generate", ignoring case
-- Returns '()'
generateKeyword :: Parser ()
generateKeyword = keywordParser "generate"

-- |Parses the keyword "generic", ignoring case
-- Returns '()'
genericKeyword :: Parser ()
genericKeyword = keywordParser "generic"

-- |Parses the keyword "guarded", ignoring case
-- Returns '()'
guardedKeyword :: Parser ()
guardedKeyword = keywordParser "guarded"

-- |Parses the keyword "if", ignoring case
-- Returns '()'
ifKeyword :: Parser ()
ifKeyword = keywordParser "if"

-- |Parses the keyword "in", ignoring case
-- Returns '()'
inKeyword :: Parser ()
inKeyword = keywordParser "in"

-- |Parses the keyword "inout", ignoring case
-- Returns '()'
inoutKeyword :: Parser ()
inoutKeyword = keywordParser "inout"

-- |Parses the keyword "is", ignoring case
-- Returns '()'
isKeyword :: Parser ()
isKeyword = keywordParser "is"

-- |Parses the keyword "label", ignoring case
-- Returns '()'
labelKeyword :: Parser ()
labelKeyword = keywordParser "label"

-- |Parses the keyword "library", ignoring case
-- Returns '()'
libraryKeyword :: Parser ()
libraryKeyword = keywordParser "library"

-- |Parses the keyword "linkage", ignoring case
-- Returns '()'
linkageKeyword :: Parser ()
linkageKeyword = keywordParser "linkage"

-- |Parses the keyword "loop", ignoring case
-- Returns '()'
loopKeyword :: Parser ()
loopKeyword = keywordParser "loop"

-- |Parses the keyword "map", ignoring case
-- Returns '()'
mapKeyword :: Parser ()
mapKeyword = keywordParser "map"

-- |Parses the keyword "mod", ignoring case
-- Returns '()'
modKeyword :: Parser ()
modKeyword = keywordParser "mod"

-- |Parses the keyword "nand", ignoring case
-- Returns '()'
nandKeyword :: Parser ()
nandKeyword = keywordParser "nand"

-- |Parses the keyword "new", ignoring case
-- Returns '()'
newKeyword :: Parser ()
newKeyword = keywordParser "new"

-- |Parses the keyword "next", ignoring case
-- Returns '()'
nextKeyword :: Parser ()
nextKeyword = keywordParser "next"

-- |Parses the keyword "nor", ignoring case
-- Returns '()'
norKeyword :: Parser ()
norKeyword = keywordParser "nor"

-- |Parses the keyword "not", ignoring case
-- Returns '()'
notKeyword :: Parser ()
notKeyword = keywordParser "not"

-- |Parses the keyword "null", ignoring case
-- Returns '()'
nullKeyword :: Parser ()
nullKeyword = keywordParser "null"

-- |Parses the keyword "of", ignoring case
-- Returns '()'
ofKeyword :: Parser ()
ofKeyword = keywordParser "of"

-- |Parses the keyword "on", ignoring case
-- Returns '()'
onKeyword :: Parser ()
onKeyword = keywordParser "on"

-- |Parses the keyword "open", ignoring case
-- Returns '()'
openKeyword :: Parser ()
openKeyword = keywordParser "open"

-- |Parses the keyword "or", ignoring case
-- Returns '()'
orKeyword :: Parser ()
orKeyword = keywordParser "or"

-- |Parses the keyword "others", ignoring case
-- Returns '()'
othersKeyword :: Parser ()
othersKeyword = keywordParser "others"

-- |Parses the keyword "out", ignoring case
-- Returns '()'
outKeyword :: Parser ()
outKeyword = keywordParser "out"

-- |Parses the keyword "package", ignoring case
-- Returns '()'
packageKeyword :: Parser ()
packageKeyword = keywordParser "package"

-- |Parses the keyword "port", ignoring case
-- Returns '()'
portKeyword :: Parser ()
portKeyword = keywordParser "port"

-- |Parses the keyword "procedure", ignoring case
-- Returns '()'
procedureKeyword :: Parser ()
procedureKeyword = keywordParser "procedure"

-- |Parses the keyword "process", ignoring case
-- Returns '()'
processKeyword :: Parser ()
processKeyword = keywordParser "process"

-- |Parses the keyword "range", ignoring case
-- Returns '()'
rangeKeyword :: Parser ()
rangeKeyword = keywordParser "range"

-- |Parses the keyword "record", ignoring case
-- Returns '()'
recordKeyword :: Parser ()
recordKeyword = keywordParser "record"

-- |Parses the keyword "register", ignoring case
-- Returns '()'
registerKeyword :: Parser ()
registerKeyword = keywordParser "register"

-- |Parses the keyword "rem", ignoring case
-- Returns '()'
remKeyword :: Parser ()
remKeyword = keywordParser "rem"

-- |Parses the keyword "report", ignoring case
-- Returns '()'
reportKeyword :: Parser ()
reportKeyword = keywordParser "report"

-- |Parses the keyword "return", ignoring case
-- Returns '()'
returnKeyword :: Parser ()
returnKeyword = keywordParser "return"

-- |Parses the keyword "select", ignoring case
-- Returns '()'
selectKeyword :: Parser ()
selectKeyword = keywordParser "select"

-- |Parses the keyword "severity", ignoring case
-- Returns '()'
severityKeyword :: Parser ()
severityKeyword = keywordParser "severity"

-- |Parses the keyword "signal", ignoring case
-- Returns '()'
signalKeyword :: Parser ()
signalKeyword = keywordParser "signal"

-- |Parses the keyword "subtype", ignoring case
-- Returns '()'
subtypeKeyword :: Parser ()
subtypeKeyword = keywordParser "subtype"

-- |Parses the keyword "then", ignoring case
-- Returns '()'
thenKeyword :: Parser ()
thenKeyword = keywordParser "then"

-- |Parses the keyword "to", ignoring case
-- Returns '()'
toKeyword :: Parser ()
toKeyword = keywordParser "to"

-- |Parses the keyword "transport", ignoring case
-- Returns '()'
transportKeyword :: Parser ()
transportKeyword = keywordParser "transport"

-- |Parses the keyword "type", ignoring case
-- Returns '()'
typeKeyword :: Parser ()
typeKeyword = keywordParser "type"

-- |Parses the keyword "units", ignoring case
-- Returns '()'
unitsKeyword :: Parser ()
unitsKeyword = keywordParser "units"

-- |Parses the keyword "until", ignoring case
-- Returns '()'
untilKeyword :: Parser ()
untilKeyword = keywordParser "until"

-- |Parses the keyword "use", ignoring case
-- Returns '()'
useKeyword :: Parser ()
useKeyword = keywordParser "use"

-- |Parses the keyword "variable", ignoring case
-- Returns '()'
variableKeyword :: Parser ()
variableKeyword = keywordParser "variable"

-- |Parses the keyword "wait", ignoring case
-- Returns '()'
waitKeyword :: Parser ()
waitKeyword = keywordParser "wait"

-- |Parses the keyword "when", ignoring case
-- Returns '()'
whenKeyword :: Parser ()
whenKeyword = keywordParser "when"

-- |Parses the keyword "while", ignoring case
-- Returns '()'
whileKeyword :: Parser ()
whileKeyword = keywordParser "while"

-- |Parses the keyword "with", ignoring case
-- Returns '()'
withKeyword :: Parser ()
withKeyword = keywordParser "with"

-- |Parses the keyword "xor", ignoring case
-- Returns '()'
xorKeyword :: Parser ()
xorKeyword = keywordParser "xor"
