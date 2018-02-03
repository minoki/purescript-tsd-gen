# purescript-tsd-gen

This is a TypeScript Definition (.d.ts) generator for PureScript.

# How to build

# How to use

# Mapping of types

## Builtin

`Prim`:

- `Function s t` (`s -> t`) `(_: s) => t`
- `Array t` `Array<t>`
- `Record` (`{ .. }`)
- `Number`, `Int`
- `String`, `Char`
- `Boolean`
- `class Partial`

`Data.Function.Uncurried`:

- `Fn0 r` `() => r`
- `Fn10 a0 a1 .. a9 r` `(_: a0, ..., _: a9) => r`

`Data.StrMap.StrMap`

- `StrMap t` `{[_: string]: t}`

## Representation of User-defined Data Types (implementation detail)

- Union of `{ $$pursType?: "<type name>", $$pursTag?: "<data constructor>", value0, ..., valueN }`
- Example: Data.Tuple, Data.Maybe, Data.Either
- Type refinement via `instanceof` should be possible.
- It mimicks nominal typing in TypeScript with these extra fields

## Representation of Abstract Data Types (implementation detail)

Hidden data constructors

- `{ $$pursType: "<type name>", $$pursTag: "<data constructor>", $$abstractMarker: never }`
    - User-code written in TypeScript cannot make a value of an abstract type by mistake.
    - Data.Void
- Data.Unit.Unit: `{ $$pursType?: "Data.Unit.Unit" }`

## Representation of Newtypes (implementation detail)

- Just an alias.

## Handling of `foreign import data`

## Higher-kinded types
