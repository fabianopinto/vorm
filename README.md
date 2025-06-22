# VORM

A minimal Common Lisp project focused on line-based geometry functionality. Built using SBCL, ASDF, and FiveAM test framework.

## Project Overview

VORM is designed as a lightweight library for working with geometric primitives in 1D and 2D space. The project provides efficient implementations of segments, lines, and parallel line structures with tolerance-aware position comparison and automatic merging of overlapping geometric entities.

### Key Features

#### Mathematical Foundation
- **Mathematical Tolerance Functions**: Precise comparison of floating-point values for both linear and angular measurements
- **Angle Normalization**: Utilities for working with angles and ensuring consistent representation
- **Custom Tolerance Settings**: Macro support for temporarily adjusting tolerance thresholds

#### 1D Geometry
- **Segment Operations**: Create, compare and merge line segments with proper floating-point tolerance
- **Line Management**: Represent collections of non-overlapping segments as unified lines
- **Empty Line Detection**: Identify empty lines for optimization and filtering
- **Automatic Optimization**: Overlapping segments are automatically merged during operations

#### 2D Geometry
- **Parallels Structure**: Set of parallel lines distributed at specific positions along an orthogonal axis
- **Position-Based Operations**: Add and manage lines at specific positions with tolerance-based comparison
- **Automatic Line Merging**: Lines at positions within tolerance are intelligently merged
- **Empty Line Filtering**: Empty lines are automatically discarded for cleaner structures

#### Shape Representation
- **Shape Structure**: Collection of parallels at different angles, representing complex 2D shapes
- **Angle-Based Organization**: Lines are grouped by their orientation angle for efficient access
- **Angle Tolerance Handling**: Similar angles are automatically merged based on angular tolerance
- **Normalized Angle Storage**: All angles are normalized to [0, 2π) for consistent representation

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
│   ├── geometry-2d.lisp        # 2D geometric primitives (parallels & shapes)
│   ├── visualization.lisp      # SVG visualization functionality
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
(defvar merged (vorm:segments-merge s1 s2))
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

;; Creating a parallels structure (set of parallel lines in 2D)
(defvar p (vorm:make-parallels (cons 1.0 line1) (cons 2.0 line2)))
;; Creates a structure with two parallel lines at positions 1.0 and 2.0

;; Adding more lines to the parallels structure
(vorm:parallels-add-lines p (cons 3.0 line3) (cons 4.0 line4))

;; Getting all positions that have lines
(vorm:parallels-positions p)  ;; => (1.0 2.0 3.0 4.0)

;; Position handling with line merging
;; When adding a line at position 1.001 which is within *linear-tolerance* of existing position 1.0,
;; the lines will be merged automatically:
(vorm:parallels-add-lines p (cons 1.001 line5))
;; Now the line at position 1.0 will be a merged version of the original and line5

;; Empty lines are automatically filtered out
(defvar empty-line (vorm:make-line))
(vorm:parallels-add-lines p (cons 5.0 empty-line)) ;; This line will be ignored

;; Creating a Shape structure (collection of parallels at different angles)
(defvar s (vorm:make-shape (cons 0.0 p1) (cons (/ pi 2) p2)))
;; Creates a shape with two sets of parallel lines at angles 0.0 and π/2

;; Adding more parallels to the shape
(vorm:shape-add-parallels s (/ pi 4) p3)

;; Getting all angles in the shape
(vorm:shape-angles s)  ;; => (0.0 0.7853982 1.5707964) [0, π/4, π/2]

;; Angle handling with parallels merging
;; When adding parallels at an angle within *angular-tolerance* of an existing angle,
;; the parallels are merged automatically:
(vorm:shape-add-parallels s (+ (/ pi 4) 0.00001) p4)
;; The parallels at π/4 will now contain the lines from both p3 and p4

;; Rendering shapes to SVG
(defvar svg-content (vorm:render-shape-to-svg shape))
;; Creates an SVG representation of the shape as a string

;; Render directly to a file
(vorm:render-shape-to-file shape "my-shape.svg" 
                         :width 800 :height 600
                         :stroke "blue" 
                         :stroke-width 2)

;; Customize appearance with additional options
(vorm:render-shape-to-file shape "styled-shape.svg"
                         :width 800
                         :height 600
                         :auto-viewbox t
                         :stroke "blue"
                         :stroke-width 2
                         :opacity 0.7)
```

## Performance Characteristics

### Algorithmic Complexity

- **Position Lookups**: O(n) complexity where n is the number of positions, with early exits for common cases
- **Line Merging**: Constant time for individual merges, linear with respect to the number of segments
- **Angle Lookups**: O(n) complexity where n is the number of angles in a shape
- **Memory Usage**: 
  - Parallels: Space complexity is O(p + s) where p is the number of positions and s is the total number of segments
  - Shapes: Space complexity is O(a * p * s) where a is the number of angles, p is average positions per angle, and s is average segments per line

### Tolerance-Based Handling

VORM uses tolerance-based comparison for robust handling of floating-point values and angles:

#### Position Tolerance
- Positions within `*LINEAR-TOLERANCE*` of each other are considered equal
- When adding a line at a position close to an existing one, the lines are automatically merged
- The merged line contains all segments from both lines, with overlapping segments combined
- Empty lines (without segments) are automatically filtered out for cleaner data structures

#### Angle Tolerance
- Angles within `*ANGULAR-TOLERANCE*` of each other are considered equal
- Angles are automatically normalized to the [0, 2π) range for consistent representation
- When adding parallels at an angle close to an existing one, the parallels are automatically merged
- The merged parallels contain all lines from both original parallels structures

### Optimization Tips

For applications with large datasets, consider these optimizations:

#### For Parallels Structures
1. **Batch Processing**: Use `parallels-add-lines` with multiple position-line pairs for better performance
2. **Position Management**:
   - Pre-sort positions when possible to minimize search time
   - Consider normalizing positions to avoid clustering that might trigger excessive merging
3. **Custom Tolerance**: Adjust `*LINEAR-TOLERANCE*` based on your application's precision requirements
4. **Empty Line Filtering**: Take advantage of automatic empty line filtering for cleaner structures

#### For Shape Structures
1. **Batch Processing**: Create shapes with all angle-parallels pairs at once when possible
2. **Angle Management**: Pre-normalize angles before adding them to minimize processing
3. **Custom Tolerance**: For applications with specific angular precision needs, adjust `*ANGULAR-TOLERANCE*`
4. **Parallels Reuse**: When possible, reuse parallels structures across angles for memory efficiency

## SVG Visualization

VORM includes functionality to render shapes to SVG format, making it easy to visualize your geometric structures.

### Basic SVG Visualization

```lisp
;; Create a simple shape
(defvar s (vorm:make-shape (cons 0.0 (vorm:make-parallels (cons 0.0 (vorm:make-line (vorm:make-segment 0.0 100.0)))))))

;; Render the shape to SVG format
(defvar svg (vorm:render-shape-to-svg s :width 500 :height 500))

;; Render directly to a file
(vorm:render-shape-to-file s "my-shape.svg")
```

### Rendering Options

```lisp
;; Customize the appearance with advanced options
(vorm:render-shape-to-file s "styled-shape.svg" 
                          :width 800 
                          :height 600 
                          :auto-viewbox t
                          :stroke "blue" 
                          :stroke-width 2 
                          :opacity 0.8)
```

### SVG Visualization Features

- **Automatic Viewbox Calculation**: Automatically determines the optimal viewBox for your shapes
- **Coordinate Transformations**: Handles all transformations from model coordinates to SVG coordinates
- **Styling Options**: Customize stroke color, width, opacity, and other SVG attributes
- **Direct File Output**: Render shapes directly to SVG files with a single function call
- **Performance Optimized**: Streamlined implementation with minimal function overhead

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

## License

MIT
