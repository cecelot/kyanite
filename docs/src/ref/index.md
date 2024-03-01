# Reference

This section contains a comprehensive reference for the Kyanite language. A representative Kyanite program might look something like this:

```kyanite
% Comments are defined with the `%` character.
const PI: float = 3.14;

Class Coordinate {
    x: int,
    y: int,
}

fun sum(c: Coordinate): int {
    return c.x + c.y;
}

fun main() {
    let coordinate: Coordinate = Coordinate:init(
        x: 1,
        y: 2,
    );

    % The `+=` form is not currently supported
    coordinate.x = coordinate.x + 1;

    println(coordinate.x);
    println(coordinate.y);

    println(sum(coordinate));
}
```
