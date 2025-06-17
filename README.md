# Vorm

A Common Lisp project structured using ASDF and FiveAM for testing with best practices in organization.

## Description

Vorm is a sample Common Lisp project that demonstrates proper project structure and
organization following Common Lisp best practices. The codebase is organized into modular components with a clean separation of concerns.

## Requirements

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/beta/) for dependency management

## Project Structure

```
vorm/
├── docs/                  # Documentation
│   ├── api/               # API documentation
│   │   ├── core.md        # Core API documentation
│   │   └── utils.md       # Utils API documentation
│   └── guides/            # User and developer guides
│       └── developer-guide.md
├── scripts/               # Utility scripts
│   ├── build-binary.lisp  # Script for building executable
│   ├── run-repl.lisp      # Script for starting REPL
│   └── run-tests.lisp     # Script for running tests
├── src/                   # Source code
│   ├── core/              # Core functionality
│   │   └── arithmetic.lisp # Arithmetic operations
│   ├── main.lisp          # Application entry point
│   ├── package.lisp       # Package definitions
│   └── utils/             # Utility functions
│       └── math.lisp      # Math utilities
└── tests/                 # Test files
    ├── core/              # Tests for core functionality
    │   └── arithmetic-test.lisp
    ├── main.lisp          # Test orchestration
    ├── package.lisp       # Test package definitions
    └── utils/             # Tests for utilities
        └── math-test.lisp
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

### Using the Executable

After building the executable with `make build`, you can run it directly:

```bash
# Show usage information
./vorm

# Add two numbers
./vorm add 5 7

# Subtract numbers
./vorm subtract 10 3
```

In the REPL, load the package to start using the functionality:

```lisp
(in-package :vorm)

(add 3 4)      ; => 7
(subtract 10 5) ; => 5
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

- [Developer Guide](docs/guides/developer-guide.md): Information for developers working with the codebase
- [Core API](docs/api/core.md): Documentation for the core functionality
- [Utils API](docs/api/utils.md): Documentation for utility functions

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
