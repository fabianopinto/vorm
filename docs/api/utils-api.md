# Utils API

The Utils module provides utility functions used throughout the Vorm system. These include list manipulation, mathematical operations, and other helper functions.

## Core Components

- [List Utilities](#list-utilities): Helper functions for working with lists
- [Math Utilities](#math-utilities): Mathematical helper functions
- [Macro Utilities](#macro-utilities): Helpful macros for common patterns

## List Utilities

### Ensure List

Ensures that a value is wrapped in a list if it's not already a list.

```lisp
(ensure-list x)
```

**Parameters**:
- `x`: Any Lisp object

**Returns**:
- `x` if it is already a list, otherwise `(list x)`

**Example**:
```lisp
(ensure-list 5)       ; => (5)
(ensure-list '(1 2 3)) ; => (1 2 3)
```

## Math Utilities

### Almost Equal

Checks if two floating point numbers are approximately equal within a tolerance.

```lisp
(almost-equal a b &optional (epsilon 1.0e-6))
```

**Parameters**:
- `a`: First number
- `b`: Second number
- `epsilon`: Optional tolerance (default: 1.0e-6)

**Returns**:
- `T` if the absolute difference between `a` and `b` is less than `epsilon`, `NIL` otherwise

**Example**:
```lisp
(almost-equal 0.1 0.10000001) ; => T
(almost-equal 1.0 1.1)        ; => NIL
```

### Point Distance

Calculates the Euclidean distance between two points.

```lisp
(point-distance x1 y1 x2 y2)
```

**Parameters**:
- `x1`: X coordinate of the first point
- `y1`: Y coordinate of the first point
- `x2`: X coordinate of the second point
- `y2`: Y coordinate of the second point

**Returns**:
- The Euclidean distance between points (x1,y1) and (x2,y2)

**Example**:
```lisp
(point-distance 0 0 3 4) ; => 5.0 (distance from origin to point (3,4))
```

### Degrees to Radians

Converts an angle from degrees to radians.

```lisp
(degrees-to-radians degrees)
```

**Parameters**:
- `degrees`: Angle in degrees

**Returns**:
- Equivalent angle in radians

**Example**:
```lisp
(degrees-to-radians 180) ; => 3.141592... (approximately π)
```

### Radians to Degrees

Converts an angle from radians to degrees.

```lisp
(radians-to-degrees radians)
```

**Parameters**:
- `radians`: Angle in radians

**Returns**:
- Equivalent angle in degrees

**Example**:
```lisp
(radians-to-degrees pi) ; => 180.0
```

## Macro Utilities

### With Gensyms

Binds each variable to a unique symbol and evaluates body, useful for macro definitions to avoid symbol capture.

```lisp
(with-gensyms (names) &body body)
```

**Parameters**:
- `names`: List of variable names to bind to gensyms
- `body`: Forms to evaluate with the gensym bindings

**Returns**:
- Result of evaluating `body` with the gensym bindings

**Example**:
```lisp
(defmacro my-when (test &body body)
  (with-gensyms (result)
    `(let ((,result ,test))
       (when ,result
         ,@body))))
```

## Functions Used By Other Modules

The utils module provides functionality that's used throughout the rest of the Vorm system:

- [Grammar API](grammar-api.md): Uses [ensure-list](#ensure-list) to handle both single shapes and lists
- [Shapes API](shapes-api.md): Uses [point-distance](#point-distance) for lines and other geometric calculations
- [Transformations API](transformations-api.md): Uses [degrees-to-radians](#degrees-to-radians) for rotation operations
- [Interpreter API](interpreter-api.md): Uses various utilities for shape manipulation
- [Parser API](parser-api.md): Uses [almost-equal](#almost-equal) when comparing coordinates

## See Also

- [Shapes API](shapes-api.md)
- [Grammar API](grammar-api.md)
- [Transformations API](transformations-api.md)
