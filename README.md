# Cidl

Cidl (for CANOpen Interface Description Language) is a simple IDL for
describing CANOpen dictionaries.

This is designed to be used in conjunction with [ivory-tower-canopen][].

[ivory-tower-canopen]: https://github.com/DistRap/ivory-tower-canopen

### Types

Cidl has a type language which permits the user to define types using
the following primitives:
- Atomic types:
    - Signed integers of 8, 16, 32, 64 bit width
    - Unsigned integers of 8, 16, 32, 64 bit width
    - IEEE 754 single and double precision floating point numbers
    - Boolean values
- User-defined Enum types:
    - Pairs of names and values, where names and values must have
      a one-to-one correspondence
    - User specified representation width (8, 16, 32, or 64 bits)
- User-defined Newtypes:
    - Wraps an existing atomic or enum type with a new type
- User-defined Records:
    - Set of named fields. Corresponds to a record or a C struct.
    - Fields may be of any other user-defined type

## About

Cidl was created for [DistRap][https://distrap.org] project by Sorki.

Cidl was inspired by [Gidl][https://github.com/DistRap/gidl]
by Pat Hickey, with help from Getty Ritter and
Trevor Elliott, as part of the [SMACCMPilot project][].

[SMACCMPilot project]: https://smaccmpilot.org
[Cauterize]: https://github.com/cauterize-tools/cauterize
