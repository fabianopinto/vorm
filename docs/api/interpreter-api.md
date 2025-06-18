# Interpreter API

The Interpreter module provides functionality for executing shape grammars in Vorm. It includes functions for one-time grammar application, step-by-step execution, tracing, and interactive grammar exploration.

## Core Components

- [Grammar Execution](#grammar-execution): Functions for applying grammars to generate shapes
- [Interactive Execution](#interactive-execution): Interactive step-by-step grammar application
- [Execution Tracing](#execution-tracing): Tracing the execution of grammars

## Grammar Execution

### Interpret Grammar

Parses and executes a grammar expression in one step.

```lisp
(interpret-grammar grammar-expr max-iterations)
```

**Parameters**:
- `grammar-expr`: A grammar expression that will be parsed into a grammar object
- `max-iterations`: Maximum number of iteration steps to perform

**Returns**:
- The final list of shapes after applying the grammar rules

**Example**:
```lisp
(interpret-grammar 
 '(grammar "my-grammar"
    (circle (50 50) 20)
    :rules ((rule (circle (50 50) 20)
                 (polygon (30 30) (70 30) (50 70))
                 :label "circle-to-triangle")))
 3)
```

**Note**: Uses [parse-grammar](parser-api.md#parse-grammar) internally to convert expressions to grammar objects.

### Generate Shapes

Generates shapes by applying a grammar for a specified number of iterations.

```lisp
(generate-shapes grammar max-iterations &key include-intermediates)
```

**Parameters**:
- `grammar`: A grammar object to apply
- `max-iterations`: Maximum number of iteration steps to perform
- `include-intermediates`: When true, returns all intermediate steps as a list

**Returns**:
- If `include-intermediates` is true, returns a list of shape lists, with each element representing the shapes after each iteration. Otherwise, returns only the final list of shapes.

**Example**:
```lisp
;; Generate final shapes after 5 iterations
(generate-shapes my-grammar 5)

;; Generate all intermediate steps
(generate-shapes my-grammar 5 :include-intermediates t)
```

**Note**: Uses the [ensure-list](utils-api.md#ensure-list) utility to handle single shapes or lists uniformly.

### Execute Grammar Script

Executes a grammar script from a file.

```lisp
(execute-grammar-script script-file &key (max-iterations 100))
```

**Parameters**:
- `script-file`: Path to the file containing a grammar expression
- `max-iterations`: Maximum number of iteration steps to perform (default: 100)

**Returns**:
- The final list of shapes after applying the grammar rules

## Interactive Execution

The interactive execution API allows for step-by-step application of grammar rules with state tracking.

### Start Interactive Grammar

Initializes interactive execution of a grammar.

```lisp
(start-interactive-grammar grammar)
```

**Parameters**:
- `grammar`: A grammar object to apply interactively

**Returns**:
- The initial set of shapes from the grammar axiom

**Example**:
```lisp
(start-interactive-grammar my-grammar)
```

### Step Interactive Grammar

Executes one step of the current interactive grammar.

```lisp
(step-interactive-grammar)
```

**Returns**:
- The new set of shapes after applying a rule, or NIL if no rules applied

**Example**:
```lisp
;; Start interactive grammar execution
(start-interactive-grammar my-grammar)
;; Execute one step and get the updated shapes
(step-interactive-grammar)
```

### Get Interactive Grammar State

Gets the current state of the interactive grammar execution.

```lisp
(get-interactive-grammar-state)
```

**Returns**:
- A property list with the current grammar execution state including:
  - `:step`: Current step number
  - `:shapes`: Current shapes after the latest step
  - `:history`: List of shape sets from each step

### Reset Interactive Grammar

Resets the interactive grammar to its initial state.

```lisp
(reset-interactive-grammar)
```

**Returns**:
- The initial set of shapes from the grammar axiom

## Execution Tracing

### Trace Grammar Execution

Executes a grammar with detailed step-by-step tracing.

```lisp
(trace-grammar-execution grammar max-iterations)
```

**Parameters**:
- `grammar`: A grammar object to apply
- `max-iterations`: Maximum number of iteration steps to perform

**Returns**:
- Two values: the final list of shapes and a list of execution traces.
  Each trace entry contains information about shapes and rules applied at each step.

**Example**:
```lisp
(multiple-value-bind (final-shapes trace-info)
    (trace-grammar-execution my-grammar 5)
  (format t "Final shape count: ~a~%" (length final-shapes))
  (format t "Steps executed: ~a~%" (length trace-info)))
```

## See Also

- [Grammar API](grammar-api.md)
- [Parser API](parser-api.md)
- [Utils API](utils-api.md) - Contains utilities used by interpreter functions
