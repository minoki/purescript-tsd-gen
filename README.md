# purescript-tsd-gen

This is a TypeScript Declaration File (.d.ts) generator for PureScript.

This tool helps you use PureScript modules from TypeScript.

# How to build

```sh
$ git clone https://github.com/minoki/purescript-tsd-gen.git
$ cd purescript-tsd-gen
$ stack install --stack-yaml=stack-purs0.15.4.yaml
```

If you are using PureScript 0.14.x or older, try following:

```sh
$ git clone -b purs0.14.x https://github.com/minoki/purescript-tsd-gen.git
$ cd purescript-tsd-gen
$ stack install --stack-yaml=stack-purs0.14.9.yaml
```

# How to use

Assuming you have compiled PureScript modules into `./output`:

```sh
$ tree output/
output/
├── Control.Alt
│   ├── externs.cbor
│   └── index.js
├── Control.Alternative
│   ├── externs.cbor
│   └── index.js
...
└── YourFancyModuleInPurs
    ├── externs.cbor
    └── index.js
```

Run the following to get the declaration files:

```sh
$ purs-tsd-gen -d output/ YourFancyModuleInPurs
```

Now you get `index.d.ts` alongside each module's `index.js`:

```sh
$ tree output/
output/
├── Control.Alt
│   ├── externs.cbor
│   ├── index.d.ts
│   └── index.js
├── Control.Alternative
│   ├── externs.cbor
│   ├── index.d.ts
│   └── index.js
...
└── YourFancyModuleInPurs
    ├── externs.cbor
    ├── index.d.ts
    └── index.js
```

# Mapping of types

## Builtin

Primitive types translates as one would imagine:

- `Function s t` (`s -> t`) --> `(_: s) => t`
- `Array t` --> `Array<t>`
    - TODO: Add an option to emit `ReadonlyArray<t>`.
- `Record { key1 :: Type1, key2 :: Type2 }` --> `{ key1: Type1, key2: Type2 }`
    - TODO: Add an option to make fields `readonly`.
- `Number`, `Int` --> `number`
- `String`, `Char` --> `string`
- `Boolean` --> `boolean`

Some modules get special handling:

- `Data.Function.Uncurried`
    - `Fn0 r` --> `() => r`
    - `Fn2 a0 a1 r` --> `(_0: a0, _1: a2) => r`
    - `Fn3 a0 a1 a2 r` --> `(_0: a0, _1: a1, _2: a2) => r`
    - ...
    - `Fn10 a0 a1 ... a9 r` --> `(_0: a0, _1: a1, ..., _9: a9) => r`
- `Effect` (from [purescript-effect](https://github.com/purescript/purescript-effect))
    - `Effect a` -> `() => a`
- `Effect.Uncurried` (from purescript-effect)
    - `EffectFn1 a0 r ` -> `(_0: a0) => r`
    - `EffectFn2 a0 a1 r` -> `(_0: a0, _1: a1) => r`
    - `EffectFn3 a0 a1 a2 r` -> `(_0: a0, _1: a1, _2: a2) => r`
    - ...
    - `EffectFn10 a0 a1 ... a9 r` -> `(_0: a0, _1: a1, ..., _9: a9) => r`
- `Data.Variant` (from [purescript-variant](https://github.com/natefaubion/purescript-variant))
    - `Variant (tag1 :: Type1, tag2 :: Type2)` --> `{type: "tag1", value: Type1} | {type: "tag2", value: Type2}`
- `Data.Nullable` (from [purescript-nullable](https://github.com/purescript-contrib/purescript-nullable))
    - `Nullable a` --> `a | null`
- `Foreign.Object` (from [purescript-foreign-object](https://github.com/purescript/purescript-foreign-object))
    - `Object t` --> `{[_: string]: t}`
- `Control.Monad.Eff` (deprecated)
    - `Eff e r` -> `() => r`
- `Data.StrMap` (deprecated)
    - `StrMap t` --> `{[_: string]: t}`

## User-defined Data Types

Data type `SomeFancyDataType :: Type -> ... -> Type -> Type` is translated to `SomeFancyDataType<a0, ..., an>`.

In contrast to usual TypeScript's structual subtyping, the translated types mimicks nominal typing with extra dummy fields.

Sum types are translated to discriminated union types, with a dummy tag field.  Type guards with `instanceof` should work.

Data constructors are typed as an object type with `new` signature and `create` or `value` field.

Types whose data constructors are not exposed, i.e. abstract types, are translated to an object type which contains `never` as a field, so that you cannot accidentally create a value of abstract types in TypeScript world.

Let's see some examples:

- Tuple

```purescript
data Tuple a b = Tuple a b
```

compiles to:

```typescript
export type /*data*/ Tuple<a, b> = Tuple$$Tuple< a, b >;
interface Tuple$$Tuple<a, b> {
    "$$pursType"?: "Data.Tuple.Tuple";
    "$$pursTag"?: "Tuple";
    value0: a;
    value1: b;
}
export const /*data ctor*/ Tuple: { create: <a, b>(_: a) => (_: b) => Tuple< a, b >; new <a, b>(_0: a, _1: b): Tuple$$Tuple< a, b > };
```

- Maybe

```purescript
data Maybe a = Nothing | Just a
```

compiles to:

```typescript
export type /*data*/ Maybe<a> = Maybe$$Nothing | Maybe$$Just< a >;
interface Maybe$$Nothing {
    "$$pursType": "Data.Maybe.Maybe";
    "$$pursTag": "Nothing";
}
export const /*data ctor*/ Nothing: { value: Maybe< any /* type variable a */ >; new (): Maybe$$Nothing };
interface Maybe$$Just<a> {
    "$$pursType": "Data.Maybe.Maybe";
    "$$pursTag": "Just";
    value0: a;
}
export const /*data ctor*/ Just: { create: <a>(_: a) => Maybe< a >; new <a>(_: a): Maybe$$Just< a > };
```

- Either

```purescript
data Either a b = Left a | Right b
```

compiles to:

```typescript
export type /*data*/ Either<a, b> = Either$$Left< a > | Either$$Right< b >;
interface Either$$Left<a> {
    "$$pursType": "Data.Either.Either";
    "$$pursTag": "Left";
    value0: a;
}
export const /*data ctor*/ Left: { create: <a, b>(_: a) => Either< a, b >; new <a>(_: a): Either$$Left< a > };
interface Either$$Right<b> {
    "$$pursType": "Data.Either.Either";
    "$$pursTag": "Right";
    value0: b;
}
export const /*data ctor*/ Right: { create: <a, b>(_: b) => Either< a, b >; new <b>(_: b): Either$$Right< b > };
```

## Newtypes

Newtypes are translated to a type synonym.  The nominal property in PureScript is lost.

## `foreign import data`

`foreign import data` are translated to `any`.

Maybe there should be a way for PS-library authors to provide corresponding `.d.ts` for foreign JavaScript modules.

## Universally Quantified Types

Simple polymorphic functions translate to generic functions.

If the type is too complex, there may situations where the emitted declarations contain undue `any` type.

## Higher-Kinded Types

Not supported.

TODO: Investigate if we can reasonably emulate higher-kinded types in TypeScript.

## Type Classes

Need more work.
