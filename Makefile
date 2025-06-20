.PHONY: all clean test load check deps

# SBCL implementation path
SBCL ?= sbcl

# Default target
all: test

# Load the system
load:
	$(SBCL) --noinform --load build/load-system.lisp --quit

# Run tests
test:
	$(SBCL) --noinform --load build/run-tests.lisp --quit

# Check for compilation warnings and style
check:
	$(SBCL) --noinform \
	  --eval "(require :asdf)" \
	  --eval "(pushnew (truename \".\") asdf:*central-registry* :test #'equal)" \
	  --eval "(handler-bind ((warning #'error)) (asdf:load-system :vorm :force t))" \
	  --eval "(format t \"~%No compilation warnings detected.~%\")" \
	  --quit

# Install dependencies
deps:
	$(SBCL) --noinform \
	  --eval "(require :asdf)" \
	  --eval "(require :quicklisp)" \
	  --eval "(ql:quickload '(:alexandria :fiveam))" \
	  --eval "(format t \"~%Dependencies installed successfully.~%\")" \
	  --quit

# Clean compiled files
clean:
	find . -type f -name "*.fasl" -delete
	find . -type f -name "*.fas" -delete
	find . -type f -name "*.lib" -delete
	find . -type f -name "*~" -delete
	find . -type f -name "*.bak" -delete

# Help command
help:
	@echo "Available targets:"
	@echo "  all     - Default target, runs tests"
	@echo "  load    - Load the system into SBCL"
	@echo "  test    - Run tests"
	@echo "  check   - Check for compilation warnings"
	@echo "  deps    - Install dependencies"
	@echo "  clean   - Clean compiled files"
	@echo "  help    - Show this help message"
