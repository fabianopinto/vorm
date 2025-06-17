.PHONY: all test repl build clean

# Set SBCL as the default Lisp implementation
LISP ?= sbcl

# Default target
all: test

# Run tests
test:
	$(LISP) --non-interactive \
		--load "scripts/run-tests.lisp" \
		--eval "(uiop:quit (if (vorm.tests:run-tests) 0 1))"

# Start a REPL with the system loaded
repl:
	$(LISP) --load "scripts/run-repl.lisp"

# Build a standalone executable
build:
	@echo "Building vorm executable..."
	$(LISP) --non-interactive \
		--load "scripts/build-binary.lisp"
	@echo "Build completed."
	@echo "You can run the executable with './vorm'"

# Clean compilation artifacts
clean:
	find . -name "*.fasl" -delete
	find . -name "*.fas" -delete
	find . -name "*.lib" -delete
	find . -name "*.o" -delete
