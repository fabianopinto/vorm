# Vorm Developer Guide

This guide provides developers with information on how to work with the Vorm project codebase.

## Project Structure

Vorm follows a modular structure for better organization and maintainability:

```
vorm/
├── docs/                  # Documentation
│   ├── api/               # API documentation
│   ├── guides/            # User and developer guides
│   └── manual.md          # User manual
├── scripts/               # Utility scripts
│   ├── run-repl.lisp      # Script to start a REPL with vorm loaded
│   └── run-tests.lisp     # Script to run tests
├── src/                   # Source code
│   ├── core/              # Core functionality
│   │   └── arithmetic.lisp # Arithmetic operations
│   ├── main.lisp          # Application entry point
│   ├── package.lisp       # Package definitions
│   └── utils/             # Utility functions
│       └── math.lisp      # Math utilities
├── tests/                 # Test files
│   ├── core/              # Tests for core functionality
│   │   └── arithmetic-test.lisp
│   ├── main.lisp          # Test orchestration
│   ├── package.lisp       # Test package definition
│   └── utils/             # Tests for utilities
│       └── math-test.lisp
├── .gitignore             # Git ignore file
├── LICENSE                # Project license (MIT)
├── Makefile               # Build and test commands
├── README.md              # Project README
└── vorm.asd               # ASDF system definition
```

## Development Workflow

### Setting up the Development Environment

1. Ensure you have SBCL installed on your system
2. Clone the repository:
   ```
   git clone https://github.com/fabianopinto/vorm.git
   cd vorm
   ```

### Running Tests

You can run the tests using the Makefile:

```
make test
```

This will run all tests and report on their status.

### Starting a REPL

To start an interactive REPL with the vorm system loaded:

```
make repl
```

When the REPL starts, you'll need to switch to the vorm package:

```lisp
(in-package :vorm)
```

Then you can use the provided functions:

```lisp
(add 10 20)      ; => 30
(subtract 50 30) ; => 20
```

### Directory Organization

- **src/core/**: Contains core functionality of the system
- **src/utils/**: Contains utility functions used throughout the system
- **tests/**: Organized to mirror the src/ directory with tests for each component
- **scripts/**: Utility scripts for development and testing

## Adding New Features

When adding new features:

1. Determine whether it belongs in `core/` or `utils/`
2. Create a new file if needed, or add to an existing module
3. Add corresponding tests in the matching directory under `tests/`
4. Update the ASDF file if you've added new files

## Coding Standards

- Follow Common Lisp naming conventions
- Document all functions with docstrings
- Ensure all new code has corresponding tests
- Keep functions focused on a single responsibility
- Use appropriate error handling
