# Transformations API

The Transformations module provides functionality for geometric transformations of shapes in Vorm. These transformations modify shapes in various ways, such as moving, rotating, scaling, or reflecting them.

## Core Components

- [Translation](#translation): Move shapes in 2D space
- [Rotation](#rotation): Rotate shapes around a center point
- [Scaling](#scaling): Scale shapes uniformly or non-uniformly
- [Reflection](#reflection): Reflect shapes across a line
- [Composition](#composition): Combine multiple transformations

## Common Interface

All transformations support a common interface through the `apply-transformation` generic function:

```lisp
(apply-transformation transformation shape)
```

**Parameters**:
- `transformation`: The transformation to apply
- `shape`: The shape to transform

**Returns**:
- A new shape that is the result of applying the transformation

## Transformation Types

### Translation

Translation moves a shape by a specified distance in the x and y directions.

```lisp
(make-translation dx dy &key name)
```

**Parameters**:
- `dx`: Distance to translate along the x-axis
- `dy`: Distance to translate along the y-axis
- `name`: Optional name for the transformation

**Accessors**:
- `translation-dx`: Get the x-axis translation distance
- `translation-dy`: Get the y-axis translation distance

**Example**:
```lisp
(make-translation 10 20) ; Translate 10 units right and 20 units down
```

### Rotation

Rotation rotates a shape around a specified center point by a specified angle.

```lisp
(make-rotation angle center-x center-y &key name)
```

**Parameters**:
- `angle`: Rotation angle in degrees (positive is counterclockwise)
- `center-x`: X coordinate of the rotation center point
- `center-y`: Y coordinate of the rotation center point
- `name`: Optional name for the transformation

**Accessors**:
- `rotation-angle`: Get the rotation angle in degrees
- `rotation-center`: Get the center point of rotation

**Example**:
```lisp
(make-rotation 90 50 50) ; Rotate 90 degrees counterclockwise around (50, 50)
```

**Note**: This function uses [degrees-to-radians](utils-api.md#degrees-to-radians) internally to convert from degrees to the radians used in calculations.

### Scaling

Scaling changes the size of a shape, either uniformly or non-uniformly.

```lisp
(make-scaling sx sy center-x center-y &key name)
```

**Parameters**:
- `sx`: Scale factor along the x-axis
- `sy`: Scale factor along the y-axis
- `center-x`: X coordinate of the scaling center point
- `center-y`: Y coordinate of the scaling center point
- `name`: Optional name for the transformation

**Accessors**:
- `scaling-sx`: Get the x-axis scale factor
- `scaling-sy`: Get the y-axis scale factor
- `scaling-center`: Get the center point of scaling

**Example**:
```lisp
(make-scaling 2.0 1.5 50 50) ; Scale 2x horizontally and 1.5x vertically around (50, 50)
```

### Reflection

Reflection reflects a shape across a specified line.

```lisp
(make-reflection line-start-x line-start-y line-end-x line-end-y &key name)
```

**Parameters**:
- `line-start-x`: X coordinate of the reflection line's start point
- `line-start-y`: Y coordinate of the reflection line's start point
- `line-end-x`: X coordinate of the reflection line's end point
- `line-end-y`: Y coordinate of the reflection line's end point
- `name`: Optional name for the transformation

**Accessors**:
- `reflection-line`: Get the reflection line

**Example**:
```lisp
(make-reflection 0 0 100 0) ; Reflect across the horizontal axis
```

### Composition

Composition combines multiple transformations into a single transformation that applies them in sequence.

```lisp
(compose-transformations transformation1 transformation2)
```

**Parameters**:
- `transformation1`: The first transformation to apply
- `transformation2`: The second transformation to apply after transformation1

**Returns**:
- A new composed transformation that applies transformation1 followed by transformation2

**Functions**:
- `identity-transformation`: Creates an identity transformation that doesn't change shapes

**Example**:
```lisp
(compose-transformations 
 (make-translation 10 0) 
 (make-rotation 45 0 0)) ; First translate, then rotate
```

## See Also

- [Shapes API](shapes-api.md)
- [Grammar API](grammar-api.md)
- [Utils API](utils-api.md) - Contains utilities like [degrees-to-radians](utils-api.md#degrees-to-radians) used in rotations
