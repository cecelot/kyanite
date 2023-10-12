# Records

Records are a **non-recursive** [^1] collection of named fields used to represent structured, similar to `struct`s in many other languages. Records are declared with the `rec` keyword, followed by a comma-separated list of field names and their types:

```kyanite
rec Coordinate {
    x: int,
    y: int,
}
```

Records are constructed as follows:

```kyanite
let coordinate: Coordinate = Coordinate:init(
    x: 1,
    y: 2,
);
```

[^1]: Records cannot contain fields of their own type, either directly (as a field on itself) or indirectly (as a field on another record that contains a field of the original record's type).