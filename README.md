# VORM

A minimal Common Lisp project focused on line-based geometry functionality. Built using SBCL, ASDF, and FiveAM test framework.

## Project Overview

VORM is designed as a lightweight library for working with geometric lines and points in a 2D space. The project is structured for clean organization with separate source files and comprehensive tests.

### Key Features

#### Mathematical Foundation
- **Mathematical Tolerance Functions**: Precise comparison of floating-point values for both linear and angular measurements
- **Angle Normalization**: Utilities for working with angles and ensuring consistent representation
- **Custom Tolerance Settings**: Macro support for temporarily adjusting tolerance thresholds

#### 1D Geometry
- **Segment Operations**: Create, compare and merge line segments with proper floating-point tolerance
- **Line Management**: Represent collections of non-overlapping segments as unified lines
- **Automatic Optimization**: Overlapping segments are automatically merged during operations

## Requirements

- [SBCL (Steel Bank Common Lisp)](https://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/) (for dependency management)

## Project Structure

```
vorm/
├── src/                        # Source code
│   ├── package.lisp            # Package definitions for VORM
│   ├── math-tolerances.lisp    # Mathematical tolerance functions
│   ├── geometry-1d.lisp        # 1D geometric primitives (segments & lines)
│   └── main.lisp               # Main code (line geometry)
├── tests/                      # Tests
│   ├── package.lisp            # Test package definitions
│   ├── math-tolerances-tests.lisp  # Tests for math tolerance functions
│   ├── geometry-1d-tests.lisp    # Tests for 1D geometry functionality
│   └── main.lisp               # Main test code
├── build/                      # Build scripts
│   ├── load-system.lisp        # Script to load the VORM system
│   └── run-tests.lisp          # Script to run VORM tests
└── vorm.asd                    # ASDF system definition
```

## Installation

1. Clone this repository
2. Ensure Quicklisp is installed and set up properly
3. Add this project to your local projects:

```bash
ln -s /path/to/vorm ~/quicklisp/local-projects/
```

## Usage

### Available Make Commands

```bash
# Install project dependencies
make deps

# Clean compiled files
make clean

# Check for compilation warnings
make check

# Run the test suite
make test
```

### Running the VORM tests

```bash
# Using the provided Makefile:
make test

# Or directly with SBCL:
sbcl --load build/run-tests.lisp
```

This will load the system, run the tests, and report the results.

### Using the VORM system in your own code

```lisp
(asdf:load-system :vorm)

;; Using mathematical tolerance functions
(vorm:linear-equal 1.0 1.000001)  ; => T (within default tolerance)
(vorm:angular-equal 0.0 (* 2 pi)) ; => T (angles are considered equivalent)

;; Using custom tolerance settings
(vorm:with-custom-tolerance (1.0e-8 1.0e-8)
  (vorm:linear-equal 1.0 1.000001)) ; => NIL (outside strict tolerance)

;; Creating segments (1D intervals)
(defvar s1 (vorm:make-segment 1.0 5.0))
(defvar s2 (vorm:make-segment 3.0 7.0))

;; Checking for segment overlap
(vorm:segments-overlap-p s1 s2)  ; => T

;; Merging segments
(defvar merged (vorm:segment-merge s1 s2))
;; merged is now a segment from 1.0 to 7.0

;; Creating lines (collections of non-overlapping segments)
(defvar line (vorm:make-line s1 s2))
;; line will contain one segment from 1.0 to 7.0 since s1 and s2 overlap

;; Adding segments to a line (automatically merges overlapping segments)
;; Note: line-add-segments takes a list of segments
(vorm:line-add-segments line (list (vorm:make-segment 10.0 15.0)))
;; line now contains two segments: [1.0,7.0] and [10.0,15.0]

;; Adding multiple segments at once
(vorm:line-add-segments line (list segment1 segment2 segment3))
;; Overlapping segments are automatically merged
```

## Development

### Project Organization

1. Source code goes in the `src` directory
   - `package.lisp` defines the package and exports
   - `main.lisp` contains the core functionality

2. Tests belong in the `tests` directory
   - Each test file should correspond to the functionality being tested
   - Use FiveAM's test macros for writing tests

### Development Workflow

1. Add new functionality to source files
   - Create new files as needed for logical separation
   - Ensure proper documentation for all public functions

2. Update exports in `package.lisp`
   - Export only the symbols intended to be part of the public API

3. Add corresponding tests
   - Write tests before or alongside implementation
   - Ensure each function has at least basic test coverage

4. Update ASDF system definition
   - If adding new files, add them to `vorm.asd` with proper dependencies

5. Run tests frequently
   ```bash
   make test
   ```

6. Check for warnings
   ```bash
   make check
   ```

## License

MIT
