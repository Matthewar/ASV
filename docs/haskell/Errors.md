# Errors
This document contains the error values and their associated user outputs.

## Top Level Error Management
The top level errors are contained in the manager [error types](../../src/Manager/Types/Error.hs) file.
They are held in the type `ConverterError` which has a number of constructors based on the modules of the simulation tool.

| Constructor | Data Contained | Description |
| :---------- | :------------- | :---------- |
| `ConverterError_Filing` | `FilingError` | Errors concerned with file checking. |
| `ConverterError_Scope` | `WrappedScopeConverterError` | Errors occuring in the scope converter, wrapped with position information. |
| `ConverterError_Parse` | `WrappedParserError` | Parsing errors (with associated position in file). |
| `ConverterError_Netlist` | `WrappedNetlistError` | Netlist conversion errors (with associated position in file). |
| `ConverterError_NotImplemented` | `PosnWrapper String` | String with details of the feature not yet implemented and associated positon in file where error occured. |
| `ConverterError_Sim` | `SimOutputError` | Errors that occur within the simulation output generator. |

## Lower Level Errors
Each of the error types defined for various components of the tool.

### Contents
- [Filing Errors](Errors/Filing.md)
- [Scope Converter Errors](Errors/Scope.md)
- Parser Errors
- Netlist Converter Errors
- [Simulation Output Generator Errors](Errors/Sim.md)

### Not Implemented Error
This is the weakest form of error, designed to be easy to remove as the features are implemented.
