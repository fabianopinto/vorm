# VORM

A minimal Common Lisp project focused on line-based geometry functionality. Built using SBCL, ASDF, and FiveAM test framework.

## Project Overview

VORM is designed as a lightweight library for working with geometric lines and points in a 2D space. The project is structured for clean organization with separate source files and comprehensive tests.

## Requirements

- [SBCL (Steel Bank Common Lisp)](https://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/) (for dependency management)

## Project Structure

```
vorm/
├── src/                # Source code
│   ├── package.lisp    # Package definitions for VORM
│   └── main.lisp       # Main code
├── tests/              # Tests
│   ├── package.lisp    # Test package definitions
│   └── main.lisp       # Test code
├── build/              # Build scripts
│   ├── load-system.lisp    # Script to load the VORM system
│   └── run-tests.lisp      # Script to run VORM tests
└── vorm.asd      # ASDF system definition
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
(vorm:dummy)
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
