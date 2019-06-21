import * as PursExample from "./output/Main";

console.log(PursExample.variantToString({type: "num", value: 123})); // => "123"
console.log(PursExample.nullableToString(null)); // => "null"
console.log(PursExample.numToSomeObj(42).bar); // => 43
PursExample.someEffectFn(23, 42); // => 65.0
