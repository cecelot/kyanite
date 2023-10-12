# Functions

Functions are declared with the `fun` keyword as follows:

```kyanite
fun foo() {}
```

`foo` takes no arguments and returns no value as output (referred to as a `void` function). The return type may also be specified explicitly (e.g. `fun foo(): void {}`).

Formal parameters are specified as follows:

```kyanite
fun foo(x: int, y: int): int {
    return x + y;
}
```

## Calling a function

Functions are called using their name followed by a list of arguments:

```kyanite
foo(1, 2);
```

Or with no arguments:
```kyanite
foo();
```