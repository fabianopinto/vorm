# Vorm User Manual

## Introduction

Vorm is a framework for creating and manipulating geometric shapes through rule-based transformations. This manual provides detailed information on how to use the system.

## Basic Shape Functions

### make-point

```lisp
(make-point x y &key id metadata)
```

Creates a point at the specified coordinates.

**Parameters:**
- `x`: X coordinate (horizontal position)
- `y`: Y coordinate (vertical position)
- `id`: Optional unique identifier
- `metadata`: Optional additional data

**Returns:**
- A new point instance

**Examples:**
```lisp
(make-point 10 20) ; Creates a point at (10, 20)
```

### make-circle

```lisp
(make-circle center-x center-y radius &key id metadata)
```

Creates a circle with the specified center and radius.

**Parameters:**
- `center-x`: X coordinate of the circle's center
- `center-y`: Y coordinate of the circle's center
- `radius`: Radius of the circle
- `id`: Optional unique identifier
- `metadata`: Optional additional data

**Returns:**
- A new circle instance

**Examples:**
```lisp
(make-circle 50 50 20) ; Creates a circle at (50, 50) with radius 20
```

## Grammar Functions

### make-grammar

```lisp
(make-grammar name axiom &key rules metadata)
```

Creates a shape grammar with the specified name and initial shape.

**Parameters:**
- `name`: A string name for the grammar
- `axiom`: The initial shape or list of shapes
- `rules`: Optional list of rules to add to the grammar
- `metadata`: Optional additional data

**Returns:**
- A new grammar instance

**Examples:**
```lisp
(make-grammar "simple-grammar" 
             (make-circle 50 50 20)
             :rules (list (make-rule ...)))
```

### generate-shapes

```lisp
(generate-shapes grammar max-iterations &key include-intermediates)
```

Generates shapes by applying a grammar for a specified number of iterations.

**Parameters:**
- `grammar`: A grammar object to apply
- `max-iterations`: Maximum number of iterations to perform
- `include-intermediates`: When true, returns all intermediate steps

**Returns:**
- A list of shapes after applying the grammar

**Examples:**
```lisp
(generate-shapes my-grammar 5) ; Apply grammar for 5 iterations
```

## Interactive Usage

VORM provides functions for step-by-step interactive grammar execution:

```lisp
;; Start interactive execution
(start-interactive-grammar grammar)

;; Execute one step
(step-interactive-grammar)

;; Get current state
(get-interactive-grammar-state)

;; Reset to initial state
(reset-interactive-grammar)
```
