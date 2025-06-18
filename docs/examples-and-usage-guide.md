# Vorm Examples and Usage Guide

## Introduction

Vorm is a framework for creating and manipulating geometric shapes through rule-based transformations. This guide provides detailed information on how to use the system, with a special focus on the included example grammars that demonstrate different applications of shape grammar concepts.

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

Vorm provides functions for step-by-step interactive grammar execution:

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

## Example Grammars

Vorm includes several example grammars that demonstrate different applications of shape grammar concepts. These examples can be found in the `examples/` directory of the project.

### Koch Curve

The Koch curve is a famous fractal curve that begins with a line segment and repeatedly replaces the middle third of each line segment with a triangular bump.

**How to Use:**

```lisp
;; Load the examples package
(asdf:load-system :vorm.examples)
(in-package :vorm.examples)

;; Generate a Koch curve with 4 iterations
(generate-koch-curve 4)
```

**Parameters:**
- The single integer parameter represents the number of iterations.
- Higher numbers create more complex fractals but may take longer to compute.
- Recommended values: 1-5 (values higher than 5 can generate very complex shapes)

**How It Works:**

The Koch curve grammar uses the following rule:
- Replace each line segment with 4 smaller segments that form a triangular bump in the middle

You can examine the grammar definition by evaluating:
```lisp
*koch-grammar*
```

### Koch Snowflake

The Koch snowflake is a variation that applies the Koch curve pattern to the sides of a triangle.

**How to Use:**

```lisp
;; Generate a Koch snowflake with 3 iterations
(generate-koch-snowflake 3)
```

**Parameters:**
- The single integer parameter represents the number of iterations.
- Recommended values: 1-4

### Square Subdivision

This example demonstrates a Mondrian-style pattern generator that recursively subdivides squares into smaller rectangles with varying proportions.

**How to Use:**

```lisp
;; Generate a square subdivision pattern with depth 3
(generate-square-subdivision 3)
```

**Parameters:**
- The parameter represents the depth of subdivision.
- Each increase in depth creates significantly more complex patterns.
- Recommended values: 1-4

**How It Works:**

The square subdivision grammar uses rules that:
1. Randomly select rectangles to split either horizontally or vertically
2. Create subdivisions with varying proportions
3. Apply color choices based on the rectangle size

You can examine the grammar definition by evaluating:
```lisp
*subdivision-grammar*
```

### Sierpinski Triangle

The Sierpinski triangle is a self-similar fractal that creates a triangular pattern with recursive holes.

**How to Use:**

```lisp
;; Generate a Sierpinski triangle with 5 iterations
(generate-sierpinski 5)
```

**Parameters:**
- The parameter represents the number of iterations.
- Each iteration increases the detail in the fractal.
- Recommended values: 1-6

### L-System Plants

This example demonstrates how to create plant-like structures using L-system based shape grammars, which are particularly well-suited for modeling plant growth and other biological forms.

**How to Use:**

```lisp
;; Generate a plant-like structure using L-system rules
(load-plant-structure)
```

**How It Works:**

The L-system plants grammar uses:
1. A turtle graphics-based interpretation system
2. Rules that simulate branching growth patterns
3. Stack operations to handle branching and returning to previous positions

You can examine the grammar definition by evaluating:
```lisp
(create-plant-grammar)
```

## Customizing Examples

You can modify and customize any of these examples by creating a copy of the grammar and adjusting the rules:

```lisp
;; Create a custom version of the Koch grammar
(defvar *my-koch-grammar* (copy-grammar *koch-grammar*))

;; Modify a rule or add a new one
(add-rule *my-koch-grammar*
          (make-rule [...your rule definition...]))

;; Use your custom grammar
(start-interactive-grammar *my-koch-grammar*)
```

## Interactive Exploration

For a deeper understanding of how each grammar works, use the interactive execution functions to step through the grammar application:

```lisp
;; Start with the Koch grammar
(start-interactive-grammar *koch-grammar*)

;; Execute a single step and see the result
(step-interactive-grammar)

;; View current state including step count
(get-interactive-grammar-state)

;; Keep stepping through or reset
(step-interactive-grammar)
(reset-interactive-grammar)
```

This approach lets you visualize exactly how each rule transforms the shapes at every step of the process.
