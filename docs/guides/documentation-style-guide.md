# Documentation Style Guide for VORM

This guide establishes consistent documentation standards for the VORM project.

## Package Documentation

Every package should have comprehensive documentation in the `defpackage` form:

```lisp
(defpackage :package-name
  (:use :cl :other-packages)
  (:documentation "Detailed description of the package's purpose, functionality, and scope.
                   Multiple lines are encouraged for complex packages.")
  (:export :exported-symbol-1
           :exported-symbol-2))
```

## Function Documentation

### Standard Function Docstring Format

```lisp
(defun function-name (parameters)
  "Short, concise description of the function's purpose.
   
   Parameters:
     PARAM1 - Description of first parameter
     PARAM2 - Description of second parameter
   
   Returns:
     Description of return value(s)
   
   Example:
     (function-name 'arg1 'arg2) ; Expected output or effect"
  ...)
```

### Grammar Function Docstring Format

For functions that create grammars or work with patterns:

```lisp
(defun create-pattern-grammar ()
  "Create a grammar that generates [pattern name/type].
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with [initial shape description] and transforms it by
   [transformation description], creating [resulting pattern description]."
  ...)
```

## Variable Documentation

```lisp
(defvar *variable-name* value
  "Description of the variable's purpose and usage.")
```

## Class Documentation

```lisp
(defclass class-name ()
  ((slot-name :initarg :slot-name
              :accessor slot-name
              :documentation "Description of the slot"))
  (:documentation "Description of the class's purpose and behavior."))
```

## File Headers

Files should begin with a header comment block:

```lisp
;;;; Module/File Name
;;;; 
;;;; This file implements [brief description].
;;;; [Additional details about the file's contents or purpose].
```

## Commenting Style

- Use `;;;;` for file headers and major section dividers
- Use `;;;` for section comments within a file
- Use `;;` for commenting code blocks
- Use `;` for end-of-line comments

## Format Consistency

- Use two spaces for indentation
- Keep lines to a reasonable length (80-100 characters recommended)
- Group related functions together
- Place exported functions before internal ones when practical
