# Entity Declaration
Becomes a module in `build/<LIBRARY>/ENTITY'<ENTITY>.hs`
- `<LIBRARY>`: Library name (upper case)
- `<ENTITY>`: Entity name (upper case)

## Imports
- Default (`STD.STANDARD`)

## Entity Header
Creates the input/output function associated with the entity

### Generics
Only _interface constant declarations_ are permitted.

Defines a record type in Haskell:
```haskell
data <ENTITY>'GENERICS =
   <ENTITY>'GENERICS
      { <entity>'generics'<GENERIC_NAME> :: <GENERIC_TYPE>
      ...
      }
```
- Name is `<ENTITY>'GENERICS`
- For each generic:
   - Entry name is `<entity>'generics'<GENERIC_NAME>`
      - `<GENERIC_NAME>`: Generic name (upper case)
      - `<entity>`: Entity name (lower case)
   - Entry type is `<GENERIC_TYPE>`
      - `<GENERIC_TYPE>`: Generic type (upper case)

If default expression included, this is dealt with on calling the function (static expression calculated in the parser).

### Ports
Only _interface signal declarations_ are permitted.

Defines two record types in Haskell:
```haskell
data <ENTITY>'PORTS'IN =
   <ENTITY>'PORTS'IN
      { <entity>'ports'in'<PORT_NAME> :: <PORT_TYPE>
      , <entity>'ports'in'<PORT_NAME>'update :: Bool
      ...
      }
data <ENTITY>'PORTS'OUT =
   <ENTITY>'PORTS'OUT
      { <entity>'ports'out'<PORT_NAME> :: <PORT_TYPE>
      , <entity>'ports'out'<PORT_NAME>'update'time :: Maybe <PORT_TYPE>
      , <entity>'ports'out'<PORT_NAME>'update'delta :: Maybe <PORT_TYPE>
      ...
      }
```
- Name is `<ENTITY>'PORTS'(IN|OUT)`
   - `IN`: For input ports
   - `OUT`: For output ports
   - NOTE: Likely that ports of mode `inout` will appear in both types
- For each port:
   - Entry name is `<entity>'ports'(in|out)'<PORT_NAME>`
      - `<PORT_NAME>`: Port name (upper case)
      - `<entity>`: Entity name (lower case)
   - Entry type is `<PORT_TYPE>`
      - `<PORT_TYPE>`: Port type (upper case)
- For each output port:
   - A second entry `<entity>'ports'out'<PORT_NAME>'changed` exists
   - This marks whether this port has changed

If default expression included, this is dealt with on calling the function (static expression calculated in the parser).
- Only can have disconnected inputs (_open_) if default expression included

Can always have disconnected output

#### Supported Port Modes
Currently only support _in_ and _out_ port modes.

## Entity Declares
Converted declares in Haskell:
- Subprogram (function/procedure)
- Types (including subtypes)
- Constants
- Signals
- Files
- Aliases
- Attributes
- Disconnects
- Use clauses

### Unsupported Declares
Unlikely to support
- Disconnects
- Use clauses
- __Maybe__ use clauses

## Entity Statements
Converted statements in Haskell:
- Concurrent assertion statements
- Procedure calls (no signal assignments)
- Process statements (no signal assignments)
