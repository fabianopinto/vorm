# Shapes API

The Shapes module provides the foundation for geometric shape creation and manipulation in Vorm. It includes basic shape definitions, properties, and operations.

## Shape Types

Vorm supports several basic shape types:

- [Point](#point): A 2D point with x and y coordinates
- [Line](#line): A line segment between two points
- [Circle](#circle): A circle defined by a center point and radius
- [Polygon](#polygon): A polygon defined by a set of vertices
- [Rectangle](#rectangle): A special case of polygon with four sides at right angles

## Common Operations

All shapes support a common set of operations:

- `shape-contains-p`: Tests if a shape contains a point
- `shape-intersects-p`: Tests if two shapes intersect
- `shape-equals-p`: Tests if two shapes are equal
- `shape-area`: Calculates the area of a shape
- `shape-perimeter`: Calculates the perimeter of a shape
- `shape-bounds`: Returns the bounding box of a shape

## Shape Documentation

### Point

A 2D point with x and y coordinates.

```lisp
(make-point x y &key id metadata)
```

**Parameters**:
- `x`: The x-coordinate (horizontal position)
- `y`: The y-coordinate (vertical position)
- `id`: Optional unique identifier for the point
- `metadata`: Optional additional data associated with the point

**Accessors**:
- `point-x`: Get the x-coordinate
- `point-y`: Get the y-coordinate

**Example**:
```lisp
(make-point 10 20) ; Creates a point at (10, 20)
```

### Line

A line segment between two points.

```lisp
(make-line start-x start-y end-x end-y &key id metadata)
```

**Parameters**:
- `start-x`: X coordinate of the line's start point
- `start-y`: Y coordinate of the line's start point
- `end-x`: X coordinate of the line's end point
- `end-y`: Y coordinate of the line's end point
- `id`: Optional unique identifier for the line
- `metadata`: Optional additional data associated with the line

**Accessors**:
- `line-start`: Get the start point
- `line-end`: Get the end point

**Functions**:
- `line-length`: Calculate the length of the line (uses [point-distance](utils-api.md#point-distance) utility)

**Example**:
```lisp
(make-line 10 20 30 40) ; Creates a line from (10, 20) to (30, 40)
```

### Circle

A circle defined by a center point and radius.

```lisp
(make-circle center-x center-y radius &key id metadata)
```

**Parameters**:
- `center-x`: X coordinate of the circle's center
- `center-y`: Y coordinate of the circle's center
- `radius`: Radius of the circle
- `id`: Optional unique identifier for the circle
- `metadata`: Optional additional data associated with the circle

**Accessors**:
- `circle-center`: Get the center point
- `circle-radius`: Get the radius

**Example**:
```lisp
(make-circle 50 50 20) ; Creates a circle at (50, 50) with radius 20
```

### Polygon

A polygon defined by a set of vertices.

```lisp
(make-polygon points &key id metadata)
```

**Parameters**:
- `points`: A list of (X Y) pairs or point objects defining the vertices
- `id`: Optional unique identifier for the polygon
- `metadata`: Optional additional data associated with the polygon

**Accessors**:
- `polygon-vertices`: Get the list of vertices

**Example**:
```lisp
(make-polygon '((10 10) (30 10) (20 30))) ; Creates a triangle
```

### Rectangle

A special case of polygon with four sides at right angles.

```lisp
(make-rectangle x y width height &key id metadata)
```

**Parameters**:
- `x`: X coordinate of the top-left corner
- `y`: Y coordinate of the top-left corner
- `width`: Width of the rectangle (horizontal size)
- `height`: Height of the rectangle (vertical size)
- `id`: Optional unique identifier for the rectangle
- `metadata`: Optional additional data associated with the rectangle

**Example**:
```lisp
(make-rectangle 10 20 50 30) ; Creates a 50x30 rectangle at (10, 20)
```

## See Also

- [Grammar API](grammar-api.md)
- [Transformations API](transformations-api.md)
- [Utils API](utils-api.md) - Contains utilities like [point-distance](utils-api.md#point-distance) used by shape functions
