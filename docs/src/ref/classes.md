# Classes

Classes are a **non-recursive** [^1] collection of named fields and methods used to represent structured data and associated operations. Classes are declared with the `class` keyword, followed by a comma-separated list of field names and their types, and optionally a collection of methods:

```kyanite
class Coordinate {
    x: int,
    y: int

    fun add(other: Coordinate): Coordinate {
        return Coordinate:init(
            x: self.x + other.x,
            y: self.y + other.y,
        );
    }
}
```

Classes are constructed as follows:

```kyanite
let coordinate: Coordinate = Coordinate:init(
    x: 1,
    y: 2,
);
```

[^1]: Classes cannot contain fields of their own type, either directly (as a field on itself) or indirectly (as a field on another class that contains a field of the original class's type).
