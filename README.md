# Vorm

A Common Lisp shape grammar system for creating and manipulating geometric forms through rule-based transformations.

## Description

Vorm is a framework for creating and manipulating geometric shapes through rule-based transformations. The system allows for the definition of shape grammars that can generate complex patterns from simple starting shapes through repeated application of transformation rules.

The project is structured following Common Lisp best practices, with ASDF for system definition and FiveAM for testing.

## Requirements

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/beta/) for dependency management

## Project Structure

```
vorm/
├── docs/                            # Documentation
│   ├── api/                         # API documentation
│   │   ├── index.md                 # API overview
│   │   ├── shapes-api.md            # Shapes documentation
│   │   ├── grammar-api.md           # Grammar documentation
│   │   ├── transformations-api.md   # Transformations documentation
│   │   ├── parser-api.md            # Parser documentation
│   │   ├── interpreter-api.md       # Interpreter documentation
│   │   └── utils-api.md             # Utils documentation
│   ├── examples-and-usage-guide.md  # Usage and examples guide
│   ├── developer-guide.md           # Developer documentation
│   ├── documentation-style-guide.md # Documentation standards
│   └── shape-grammar-concepts.md    # Shape grammar concepts
├── examples/                        # Example shape grammar definitions
│   ├── koch-curve.lisp              # Koch curve and snowflake fractals
│   ├── sierpinski.lisp              # Sierpinski triangle fractal
│   ├── square-subdivision.lisp      # Mondrian-style patterns
│   ├── l-system-plants.lisp         # Plant-like L-system structures
│   └── package.lisp                 # Examples package definition
├── scripts/                         # Utility scripts
│   ├── build-binary.lisp            # Script for building executable
│   ├── run-repl.lisp                # Script for starting REPL
│   └── run-tests.lisp               # Script for running tests
├── src/                             # Source code
│   ├── shapes.lisp                  # Basic geometric shapes
│   ├── grammar.lisp                 # Shape grammar rules system
│   ├── transformations.lisp         # Geometric transformations
│   ├── parser.lisp                  # Grammar expression parser
│   ├── interpreter.lisp             # Grammar interpreter
│   ├── utils.lisp                   # Utility functions
│   ├── main.lisp                    # Application entry point
│   └── package.lisp                 # Package definitions
└── tests/                           # Test files
    ├── shapes-test.lisp             # Tests for shapes
    ├── grammar-test.lisp            # Tests for grammar
    ├── main.lisp                    # Test orchestration
    └── package.lisp                 # Test package definitions
```

## Installation

Clone the repository:

```bash
git clone https://github.com/fabianopinto/vorm.git
cd vorm
```

Load the system with ASDF:

```lisp
(asdf:load-system :vorm)
```

## Usage

The project provides command-line tools for common operations:

```bash
# Run tests
make test

# Start a REPL with vorm loaded
make repl

# Build a standalone executable
make build

# Clean up compiled files
make clean
```

### Using the REPL

The most convenient way to use VORM is through the REPL. Start it with:

```bash
# Start a REPL with vorm loaded
make repl
```

In the REPL, you'll be automatically placed in the vorm package. Here are some examples of what you can do:

```lisp
;; Create basic shapes
(make-point 10 20)                     ; Create a point at (10,20)
(make-line 10 20 30 40)                ; Create a line from (10,20) to (30,40)
(make-circle 50 50 20)                 ; Create a circle at center (50,50) with radius 20
(make-polygon '((10 10) (30 10) (20 30))) ; Create a triangle

;; Create and apply a simple grammar
(defvar *my-grammar* (parse-grammar
  '(grammar "my-grammar"
     (circle (50 50) 20)
     :rules ((rule (circle (50 50) 20)
                  (polygon (30 30) (70 30) (50 70))
                  :label "circle-to-triangle")))))

;; Generate shapes by applying the grammar for 3 iterations                  
(generate-shapes *my-grammar* 3)
```

### Using the Executable

After building the executable with `make build`, you can run it directly to see available commands:

```bash
# Show usage information
./vorm

# Display shape operation capabilities
./vorm shapes

# Show help information
./vorm help
```

## Testing

Tests are implemented using the FiveAM testing framework and organized by component:

```lisp
# Run tests from command line
make test

# Or within Lisp
(asdf:test-system :vorm)
```

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- [Examples and Usage Guide](docs/examples-and-usage-guide.md): Detailed instructions and examples for using Vorm
- [API Documentation](docs/api/index.md): Complete reference for all modules
- [Developer Guide](docs/developer-guide.md): Information for developers working with the codebase
- [Shape Grammar Concepts](docs/shape-grammar-concepts.md): Overview of shape grammar concepts in Vorm
- [Documentation Style Guide](docs/documentation-style-guide.md): Standards for code documentation

Additionally, the codebase itself contains comprehensive docstrings with examples, parameter descriptions, and cross-references to related functions.

## Key Features

- Basic shape creation (points, lines, polygons, circles, rectangles)
- Shape grammar rule definition and pattern matching
- Geometric transformations (translation, rotation, scaling, reflection)
- Interactive and step-by-step grammar execution
- S-expression based grammar definition language
- Example shape grammars (Koch curve, Sierpinski triangle, etc.)
- Comprehensive documentation and test suite

## Example Grammars

Vorm includes several example grammars in the `examples/` directory that demonstrate different applications of shape grammars:

| Example | Description | Function | 
|---------|-------------|----------|  
| **Koch Curve** | A famous fractal curve that replaces line segments with triangular bumps | `(generate-koch-curve 4)` |
| **Koch Snowflake** | Applies the Koch curve pattern to the sides of a triangle | `(generate-koch-snowflake 3)` |
| **Square Subdivision** | Creates Mondrian-style patterns by recursively subdividing rectangles | `(generate-square-subdivision 3)` |
| **Sierpinski Triangle** | Creates a self-similar triangular pattern with recursive holes | `(generate-sierpinski 5)` |
| **L-System Plants** | Generates plant-like structures using L-system grammars | `(load-plant-structure)` |

To use any of these examples:

```lisp
;; Load the example package
(asdf:load-system :vorm.examples)

;; Switch to the examples package
(in-package :vorm.examples)

;; Run the example of your choice
(generate-koch-curve 4)
```

For detailed instructions on each example, parameter meanings, and how to customize them, see the [Examples and Usage Guide](docs/examples-and-usage-guide.md).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
