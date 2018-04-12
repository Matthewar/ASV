# Generating haskell packages for VHDL builtins

## E.g. Standard
Potential implementation of standard library conversion:
```
module Standard where

-- predefined enumeration types:

-- |Boolean
-- > type BOOLEAN is (FALSE,TRUE);
type Boolean = Bool

-- |Bit
-- > type BIT is ('0','1');
data Bit = Bit_Zero | Bit_One

-- ?? type Character is 

data Severity_Level = Severity_Level_Note | Severity_Level_Warning | Severity_Level_Error | Severity_Level_Failure

-- predefined numeric types:

type Universal_Integer = Int64

type
```
