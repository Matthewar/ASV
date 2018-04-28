# Object Declarations and Types
## Object Classes
### Constants
```haskell
constant'<CONSTANT> :: <TYPE>
constant'<CONSTANT> = <CALCULATION>
```
- `<CONSTANT>`: Constant name (upper case)
- `<TYPE>`: Constant type (upper case)
- `<CALCULATION>`: Expression to get value

NOTE: Cannot be access or file type

#### Valid Constant Objects
- (Formal) subprogram parameters of mode in _can_ be
- Local and formal generics **must** be
- Loop/generate indices within a loop/generate statement
- Subelement or slice of a constant is a constant

### Signals
Signals become part of the state of the current entity in which they exist unless they are in a package.
If in a package, they are simply constants with signal types/attributes.
```haskell
data Control'Signal a =
   Control'Signal
      { control'signal'current :: (Control'Time,a)
      , control'signal'transactions :: [(Control'Time,a)]
      }
-- If in a package
signal'<SIGNAL> :: Control'Signal <TYPE>
signal'<SIGNAL> = <CALCULATION>
```
- `<SIGNAL>`: Signal name (upper case)
- `<TYPE>`: Signal type (upper case)
- `<CALCULATION>`: Expression to get (initial) value
   - If default expression not included, value is leftmost

If not in a package, signal is part of the state and `<CALCULATION>` gives the initial state values.

NOTE: Cannot be access or file type

#### Queueing Signal Assignments
Signal assignments are queued after some time period as suggested by the output waveform expression `after time_expression`.
- Time cannot be negative
- If `after 0ns` (implicit if not included) time is after 1 delta

#### Array Types
For array types, `Control'Signal` is defined for the highest element with a resolution function, and the array is wrapped around this if necessary.
If there are no resolution functions, the type is defined for the lowest element.

#### Resolved Signals
If a signal has an associated resolution function, it is a resolved signal IE it can have multiple drivers.
This is associated with the signal by means of the type class:
```haskell
class VHDL_Resolved a where
   resolved :: a -> a -> a
```

#### Guarded Signals
- Not supported.
- When signal kind is specified
- Must be resolved.
- Has a guard expression of BOOLEAN type
   - When guard expression is BOOLEAN'IDEN'FALSE, signal is turned off

#### Use of Signals
- If a subelement of a resolved composite signal is an actual in a port map when mode is not **in**
   - Then every subelement of that signal must be associated in that port map
   - Same mode restrictions for other subelements
   - Must be associated with a single subelement each
   - Collection of subelements from port clause are one signal source (used in resolution function)
   - If a resolved composite signal is an actual in a port map, this is the same as having each one separately
- If a subelement of a resolved composite signal has a driver in a process
   - All subelements must be driven in that process
   - Collection of these drivers forms a resolution source

#### Valid Signal Objects
Also includes ports of any mode

#### Other Considerations
- Transport type
- Assigning to signal aggregate

### Variables
Variables are part of the state of the current entity in which they exist.
- See state docs for Haskell snippets.

NOTE: Cannot be file type

#### Initial State Value
- Provided by default expression
- If default expression not included, leftmost value
   - For access, default initial value is null (not implemented)

#### Use of Variables
- Mode _in_: file variables
- Mode _out_ or _inout_: variables of any kind

#### Persistance
- Procedure: Initialised in procedure, information lost on procedure end
- For processes, variables persist for all of time

## Files
TBD

## Interface Declarations
- Constants: Generics of design entities, components, blocks; subprogram parameters
- Signals: Ports of design entities, components, blocks; subprogram parameters
- Variables: Subprogram parameters

### Constants
Only permitted to be mode in
- Becomes inputs to functions

### Signals
Dealt with by passing states.

### Variables
Dealt with by passing states.

### Modes
Mode _in_ is always default.

#### In
- Value can be read (obviously)
   - Cannot write to it
- Read all attributes
   - Except within a subprogram, cannot read:
      - Stable
      - Quiet
      - Delayed
      - Transaction
- Allowed to endfile files (not implemented)

#### Out
- Updating allowed
- Reading not allowed
   - Can read attributes, except:
      - Stable
      - Quiet
      - Delayed
      - Transaction
      - Event
      - Active
      - Last_event
      - Last_active
      - Last_value
- Allowed to endfile files (not implemented)

#### Inout
- Can read and update
- Can read all attributes
- Allowed to endfile files (not implemented)

#### Buffer
TBD

#### Linkage
TBD

### Uses of Interfaces
- Generics: Only constants
- Ports: Only signals
- Parameters: Any

## Loop/Generate Indices
## Subprogram Parameters (Formal)
## Entity Ports (Formal)
## Generics (Formal)
## Ports (Local)
## Generics (Local)
## Elements/Slices of Objects
## Objects from access types
TBD.
