# Core Module API Documentation

## Overview

The Core module contains the fundamental functionality of the Vorm system, including basic arithmetic operations. These operations are implemented in `src/core/arithmetic.lisp`.

## Functions

### `add`

```lisp
(add a b)
```

**Description:**
Adds two numbers together.

**Parameters:**
- `a`: First number
- `b`: Second number

**Returns:**
- The sum of `a` and `b`

**Examples:**
```lisp
(add 2 3)  ; => 5
(add -1 5) ; => 4
(add 0.3 0.2) ; => 0.5
```

### `subtract`

```lisp
(subtract a b)
```

**Description:**
Subtracts the second number from the first.

**Parameters:**
- `a`: Number to subtract from
- `b`: Number to subtract

**Returns:**
- The result of `a - b`

**Examples:**
```lisp
(subtract 10 4) ; => 6
(subtract 5 8)  ; => -3
(subtract 0.3 0.2) ; => 0.1
```

## Error Handling

The core functions do not currently perform special error handling beyond what is provided by the Common Lisp arithmetic operations. Invalid inputs (e.g., non-numbers) will result in standard Common Lisp type errors.

## Future Extensions

Future versions of the Core module may include:
- Multiplication and division operations
- More advanced mathematical functions
- Support for different numeric types
