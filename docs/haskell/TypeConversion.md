# Type Conversion Rules
There are two types of type conversion (explicit and implicit).

## Explicit
Used by writing `<type_name>'(<expression>)` to convert the expression to a particular type.

- Type of expression must be determinable independent of target type (obviously, the context cues are removed by forcing it to convert to a particular type)
- Expression cannot be **null**, allocator (not implemented), aggregate, or string literal
- Expression must be able to exist alone

### Conversion to Subtype
If type name indicates a subtype then
- Convert to target type (base type of subtype)
- Check that result of conversion is within subtype constraints
   - If not, error

### Allowed Conversions
Types must be **closely** related
- Can convert to own type
- Integer and floating point types
   - Can convert to another other integer or floating point type
   - Float to integer: Round to nearest unless halfway, in which case implementation decides
- Arrays:
   - Must both be arrays (target and original)
   - Must have same dimensions and lengths
   - Indices must be convertible
      - If unconstrained: Convert index to new type
         - Check that conversion is within subtype constraints, if not, error
      - If constrained: Indices determined by constraint
         - Must have an element in target for every expression element, and vice versa, otherwise error
   - Element types are the same

## Implicit
Conversions without explicitly stating them
- Universal integers (unwrapped) to any integer type
- Universal real (unwrapped floats) to any real type

## Subtypes
NOTE: Subtypes are **not** a new type.

They are a subtype of the base type of the indicated type name.
