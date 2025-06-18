# Vorm API Documentation

This section provides comprehensive documentation for the Vorm shape grammar system API.

## Overview

Vorm is a framework for creating and manipulating geometric shapes through rule-based transformations. The system allows for the definition of shape grammars that can generate complex patterns from simple starting shapes.

## API Sections

The API documentation is organized by major system components:

- [Shapes API](shapes-api.md): Basic shape definitions and operations
- [Grammar API](grammar-api.md): Grammar definition and rule application
- [Transformations API](transformations-api.md): Geometric transformations for shapes
- [Parser API](parser-api.md): Parsing of shape grammar expressions
- [Interpreter API](interpreter-api.md): Execution of shape grammars
- [Utils API](utils-api.md): Utility functions

## Getting Started

To get started with Vorm, the following example creates a simple grammar and applies it:

```lisp
;; Create a simple grammar that turns a circle into a triangle
(defvar *my-grammar* (parse-grammar
  '(grammar "my-grammar"
     (circle (50 50) 20)
     :rules ((rule (circle (50 50) 20)
                  (polygon (30 30) (70 30) (50 70))
                  :label "circle-to-triangle")))))

;; Apply the grammar for 3 iterations
(generate-shapes *my-grammar* 3)
```

For a more interactive experience, you can use the step-by-step execution interface:

```lisp
;; Start interactive execution
(start-interactive-grammar grammar)

;; Execute one step at a time
(step-interactive-grammar)
```

Refer to the specific API sections for detailed information on each component of the system.
