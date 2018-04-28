# Timing, Events and Simulation Top Level Control
## Timing
There are two elements of time:
- VHDL time: Value of VHDL STD.STANDARD library type TIME
   - Implemented as a counter of `Int64` in Haskell representing the lowest level of VHDL time `fs`
- Delta: Value representing current position within a single timeframe
   - This allows data to flow between operations which are not clocked
   - Will have a timeout value to prevent cyclic loops

## Updating Waves
Deltas do not represent real time.
At the end of a VHDL time period (when the transition occurs from one time period to the next) the waves are updated.

NOTE: This won't display hazards _?? would this be wanted_

## State
To record current state and calculate continuation data (when signal has changed and will need to trigger other events).
```haskell
data Control'Time =
   Control'Time
      { time'Real :: TIME
      , time'Delta :: Int
      }
   deriving (Eq)
instance Ord Control'Time where
   compare val1 val2
      | time'Real val1 < time'Real val2 = LT
      | time'Real val1 > time'Real val2 = GT
      | time'Real val1 == time'Real val2 = compare (time'Delta val1) (time'Delta val2)
data META'Events portsInType stateType =
   META'Events
      { entity'PortsIn :: portsInType
      , entity'State :: stateType
      , queue :: [(TIME,
      }
```

## Monad Stack
Low level stack:
```haskell
-- |Monad stack
-- Access to IO for file access/stdin/stdout
type Control'Stack a = StateT Control'SEVERITY_TRACKER (ExceptT Control'SEVERITY_FAILURE IO) a

-- |Track number of non-fatal asserts that have occurred
data Control'SEVERITY_TRACKER =
   Control'SEVERITY_TRACKER
      { control'warnings :: Int
      , control'errors :: Int
      }

-- |Fail on SEVERITY_LEVEL'IDEN'FAILURE assert
data Control'SEVERITY_FAILURE = Control'SEVERITY_FAILURE String
```
