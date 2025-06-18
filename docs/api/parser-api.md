# Parser API

The Parser module provides functionality for parsing shape grammar expressions in Vorm. It converts s-expressions representing shapes, rules, and grammars into corresponding Vorm objects.

## Core Components

- [Shape Parsing](#shape-parsing): Parsing shape expressions
- [Transformation Parsing](#transformation-parsing): Parsing transformation expressions
- [Rule Parsing](#rule-parsing): Parsing rule expressions
- [Grammar Parsing](#grammar-parsing): Parsing grammar expressions

## Shape Parsing

### Parse Shape

Parses a shape expression into a shape object.

```lisp
(parse-shape expr)
```

**Parameters**:
- `expr`: A shape expression, which can be one of:
  - A symbol (variable or reference)
  - A shape expression like `(point x y)`, `(line (x1 y1) (x2 y2))`, etc.

**Returns**:
- A shape object representing the parsed expression, or the symbol if a variable

**Example**:
```lisp
(parse-shape '(circle (50 50) 20)) ; Creates a circle at (50,50) with radius 20
(parse-shape '(polygon (10 10) (30 10) (20 30))) ; Creates a triangle
(parse-shape '?x) ; Returns the symbol ?x (represents a variable)
```

### Shape Type Parsers

Specific functions for parsing different types of shapes:

#### Parse Point

```lisp
(parse-point expr)
```

**Parameters**:
- `expr`: A point expression `(point x y)`

**Returns**:
- A point object

#### Parse Line

```lisp
(parse-line expr)
```

**Parameters**:
- `expr`: A line expression `(line (x1 y1) (x2 y2))`

**Returns**:
- A line object

#### Parse Polygon

```lisp
(parse-polygon expr)
```

**Parameters**:
- `expr`: A polygon expression `(polygon (x1 y1) (x2 y2) ...)`

**Returns**:
- A polygon object

#### Parse Circle

```lisp
(parse-circle expr)
```

**Parameters**:
- `expr`: A circle expression `(circle (center-x center-y) radius)`

**Returns**:
- A circle object

#### Parse Rectangle

```lisp
(parse-rectangle expr)
```

**Parameters**:
- `expr`: A rectangle expression `(rectangle x y width height)`

**Returns**:
- A rectangle object

## Transformation Parsing

### Parse Transformation

Parses a transformation expression into a transformation object.

```lisp
(parse-transformation expr)
```

**Parameters**:
- `expr`: A transformation expression

**Returns**:
- A transformation object representing the parsed expression

### Transformation Type Parsers

Specific functions for parsing different types of transformations:

#### Parse Translation

```lisp
(parse-translation expr)
```

**Parameters**:
- `expr`: A translation expression `(translate dx dy)`

**Returns**:
- A translation object

#### Parse Rotation

```lisp
(parse-rotation expr)
```

**Parameters**:
- `expr`: A rotation expression `(rotate angle center-x center-y)`

**Returns**:
- A rotation object

**Note**: Uses [degrees-to-radians](utils-api.md#degrees-to-radians) to convert angle input.

#### Parse Scaling

```lisp
(parse-scaling expr)
```

**Parameters**:
- `expr`: A scaling expression `(scale sx sy center-x center-y)`

**Returns**:
- A scaling object

#### Parse Reflection

```lisp
(parse-reflection expr)
```

**Parameters**:
- `expr`: A reflection expression `(reflect line-x1 line-y1 line-x2 line-y2)`

**Returns**:
- A reflection object

## Rule Parsing

### Parse Rule

Parses a rule expression into a rule object.

```lisp
(parse-rule expr)
```

**Parameters**:
- `expr`: A rule expression of the form:
  ```
  (rule left-side right-side &key label probability condition)
  ```

**Returns**:
- A rule object representing the parsed expression

**Example**:
```lisp
(parse-rule '(rule (circle (50 50) 20)
                  (polygon (30 30) (70 30) (50 70))
                  :label "circle-to-triangle"
                  :probability 0.8
                  :condition (> ?r 10)))
```

## Grammar Parsing

### Parse Grammar

Parses a grammar expression into a grammar object.

```lisp
(parse-grammar expr)
```

**Parameters**:
- `expr`: A grammar expression of the form:
  ```
  (grammar name axiom &key rules metadata)
  ```

**Returns**:
- A grammar object representing the parsed expression

**Example**:
```lisp
(parse-grammar 
 '(grammar "my-grammar"
           (circle (50 50) 20)
           :rules ((rule (circle (50 50) 20)
                        (polygon (30 30) (70 30) (50 70))
                        :label "circle-to-triangle"))))
```

## See Also

- [Shapes API](shapes-api.md)
- [Grammar API](grammar-api.md)
- [Interpreter API](interpreter-api.md)
- [Utils API](utils-api.md) - Contains utilities used in parsing
