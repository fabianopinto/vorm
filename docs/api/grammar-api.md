# Grammar API

The Grammar module provides functionality for defining and working with shape grammars in Vorm. A shape grammar consists of rules that transform shapes according to pattern matching and substitution.

## Core Components

- [Rules](#rules): Define pattern matching and replacement operations
- [Grammar](#grammar): Collection of rules with an initial shape (axiom)
- [Pattern Matching](#pattern-matching): Mechanism for matching shapes against patterns
- [Variable Binding](#variable-binding): System for binding variables in patterns

## Rules

A rule defines a transformation from one shape (or pattern) to another.

```lisp
(make-rule left-side right-side &key id label condition probability)
```

**Parameters**:
- `left-side`: The pattern to match against shapes
- `right-side`: The replacement shape or shapes
- `id`: Optional unique identifier for the rule
- `label`: Optional descriptive label for the rule
- `condition`: Optional function that must evaluate to true for rule to be applied
- `probability`: Value between 0.0 and 1.0 indicating application probability (default: 1.0)

**Accessors**:
- `rule-left-side`: Get the left side (pattern) of the rule
- `rule-right-side`: Get the right side (replacement) of the rule
- `rule-label`: Get the label of the rule
- `rule-probability`: Get the probability of the rule
- `rule-condition`: Get the condition function of the rule

**Example**:
```lisp
(make-rule 
 (make-circle 50 50 20)
 (make-polygon '((30 30) (70 30) (50 70)))
 :label "circle-to-triangle"
 :probability 0.8)
```

## Grammar

A grammar is a collection of rules and an initial shape (axiom).

```lisp
(make-grammar name axiom &key rules metadata)
```

**Parameters**:
- `name`: A string name for the grammar
- `axiom`: The initial shape or list of shapes
- `rules`: Optional list of rules to add to the grammar
- `metadata`: Optional additional data associated with the grammar

**Accessors**:
- `grammar-name`: Get the name of the grammar
- `grammar-axiom`: Get the axiom of the grammar
- `grammar-rules`: Get the list of rules in the grammar

**Functions**:
- `add-rule`: Add a rule to a grammar
- `apply-rule`: Apply a rule to a shape
- `apply-grammar-step`: Apply one step of a grammar to shapes
- `apply-grammar`: Apply a grammar for multiple iterations

**Example**:
```lisp
(make-grammar "simple-grammar"
             (make-circle 50 50 20)
             :rules (list (make-rule 
                          (make-circle 50 50 20)
                          (make-polygon '((30 30) (70 30) (50 70)))
                          :label "circle-to-triangle")))
```

## Pattern Matching

Pattern matching is the process of finding shapes that match a specified pattern.

### Variables in Patterns

Patterns can include variables (symbols starting with '?') that can match any value and be bound for use in the replacement side of a rule.

**Example**:
```lisp
;; A rule that matches any circle and replaces it with a square
;; of the same size at the same location
(make-rule
 '(circle (?x ?y) ?r)
 '(rectangle (- ?x ?r) (- ?y ?r) (* ?r 2) (* ?r 2)))
```

In this example:
- `?x` and `?y` are variables that will bind to the center coordinates of a circle
- `?r` is a variable that will bind to the radius of the circle
- The replacement uses these variables with mathematical operations like [- and *](utils-api.md#math-utilities)

### Match-Shape Function

```lisp
(match-shape pattern shape &optional bindings)
```

**Parameters**:
- `pattern`: The pattern to match
- `shape`: The shape to match against
- `bindings`: Optional existing bindings to extend

**Returns**:
- An alist of variable bindings if the match is successful, or NIL if no match

## Variable Binding

Variable binding is the process of associating pattern variables with concrete values during pattern matching.

### Substitute-Bindings Function

```lisp
(substitute-bindings shape bindings)
```

**Parameters**:
- `shape`: A shape or pattern containing variables
- `bindings`: An alist mapping variable symbols to values

**Returns**:
- A new shape with all variables replaced by their bound values

**Note**: This function uses the [ensure-list](utils-api.md#ensure-list) utility to handle single shapes or lists of shapes uniformly.

## See Also

- [Shapes API](shapes-api.md)
- [Parser API](parser-api.md)
- [Interpreter API](interpreter-api.md)
- [Utils API](utils-api.md) - Contains utilities like [almost-equal](utils-api.md#almost-equal) used in pattern matching
