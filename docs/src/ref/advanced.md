# Inheritance and Generics

Kyanite has preliminary support for inheritance and generics. The following is the syntax for extending a class:

```
class X {
    x: int

    fun show(self) {
        println_int(self.x);
    }
}

class Z: X {
    y: int

    fun show(self) {
        println_int(self.x);
        println_int(self.y);
    }
}
```

As shown, child classes inherit fields of parent classes. Additionally, the child class's `show` function overrides the parent class's `show` function.

## Generics

Suppose we have a class `Print`:

```
class Print {
    fun print(self) { /* not implemented */ }
}
```

Then, a class `Foo` can be declared like so:

```
class Foo<T: Print>: Print {
    val: T

    fun print(self) {
        self.val.print();
    }
}
```

The type parameter `T` has a bound of `Print`, meaning Foo can act as a container of sorts for any `Print`able object. Becuase `T` is bounded, we can access `val.print()` inside the body of `Foo`'s print method.

Unbounded type parameters are also supported when the type of the object is not important to the program's logic.
