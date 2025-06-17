# Utilities Module API Documentation

## Overview

The Utilities module contains helper functions and tools that support the core operations of the Vorm system. These utilities are designed to be reusable across different parts of the application.

## Math Utilities

### `approx=`

```lisp
(approx= a b &optional (epsilon 0.00001))
```

**Description:**
Compares two floating-point numbers for approximate equality within a specified epsilon value. This is useful for floating-point comparisons where exact equality might fail due to rounding errors.

**Parameters:**
- `a`: First floating-point number
- `b`: Second floating-point number
- `epsilon`: Optional tolerance value, defaults to 0.00001

**Returns:**
- `T` if the absolute difference between `a` and `b` is less than or equal to `epsilon`
- `NIL` otherwise

**Examples:**
```lisp
(approx= 0.1 0.1)         ; => T
(approx= 0.1 0.10000001)  ; => T (within default epsilon)
(approx= 0.1 0.2)         ; => NIL (difference exceeds default epsilon)
(approx= 1.0 1.05 0.1)    ; => T (within custom epsilon of 0.1)
(approx= 1.0 1.05 0.01)   ; => NIL (exceeds custom epsilon of 0.01)
```

## Usage Notes

The `approx=` function is particularly useful when dealing with:

1. Floating-point arithmetic where precision errors can occur
2. Comparing the results of calculations that might have small rounding differences
3. Testing numerical algorithms where exact equality is too strict

## Integration with Testing

The utilities are designed to work seamlessly with the testing framework. The `approx=` function is used in tests to verify floating-point results without being affected by minor precision differences.

Example in tests:
```lisp
(test test-floating-point
  (is (approx= (some-calculation) expected-result)))
```
