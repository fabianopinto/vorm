# Shape Grammar Concepts

This document provides an overview of shape grammar concepts as implemented in the VORM system.

## What are Shape Grammars?

Shape grammars are a class of generative systems that use rules to transform shapes. They were originally developed by George Stiny and James Gips in the 1970s as a formalism for architectural design. Shape grammars provide a formal method for creating complex geometric designs through the recursive application of transformation rules.

## Core Components

### Shapes

In VORM, shapes are geometric entities like points, lines, polygons, and circles. Each shape has properties that define its geometry:

- **Points**: Defined by x and y coordinates
- **Lines**: Defined by start and end points
- **Polygons**: Defined by a sequence of vertices
- **Circles**: Defined by a center point and radius

### Rules

A shape grammar rule consists of:

1. **Left side (pattern)**: A shape or pattern to match
2. **Right side (replacement)**: A shape or set of shapes to replace the matched pattern
3. **Optional conditions**: Additional constraints that must be satisfied for the rule to apply
4. **Optional probability**: A value indicating the likelihood of rule application

When a rule is applied, instances of the pattern in the design are replaced with the corresponding replacement.

### Grammar

A grammar consists of:

1. **Axiom**: The initial shape(s) to start with
2. **Rules**: A set of transformation rules
3. **Control strategy**: The method for applying rules (in VORM, typically top-down)

## Rule Application Process

The process of applying a grammar involves:

1. **Matching**: Finding instances of rule patterns in the current set of shapes
2. **Binding**: Creating variable bindings that map pattern elements to actual shapes
3. **Transforming**: Replacing matched patterns with their corresponding replacements
4. **Iteration**: Repeating the process until no more rules apply or a maximum number of iterations is reached

## Shape Grammar Formalism

Formally, a shape grammar G is defined as a 4-tuple:

G = (VT, VM, R, I)

Where:
- VT is a finite set of terminal shapes
- VM is a finite set of marker shapes (used to control rule application)
- R is a finite set of shape rules of the form α → β
- I is the initial shape (axiom)

## Variables and Parameterization

VORM supports parameterized shape grammars through variables. Variables in rules allow for:

1. **Pattern matching**: Finding shapes with specific relationships
2. **Consistency**: Ensuring the same shape appears multiple times
3. **Parameterization**: Creating rules that apply to shapes with different dimensions

Variables in VORM are symbols starting with '?' (e.g., `?x`, `?width`).

## Common Applications

Shape grammars have been applied to various domains:

1. **Architecture**: Generating building designs and analyzing architectural styles
2. **Art and design**: Creating patterns and decorative motifs
3. **Computer graphics**: Generating complex textures and geometric models
4. **Urban planning**: Analyzing and generating urban layouts
5. **Product design**: Creating variations of products with consistent styles
6. **Biological modeling**: Simulating growth patterns in plants and organisms

## Types of Shape Grammars

The VORM system supports several types of shape grammars:

### Standard Shape Grammars

These apply rules based on direct geometric matching and replacement.

### Parametric Shape Grammars

These use variables to define rules that can match and transform shapes with varying dimensions.

### Stochastic Shape Grammars

These include probabilities for rule application, introducing randomness into the generative process.

## L-Systems vs. Shape Grammars

Lindenmayer systems (L-systems) are another type of formal grammar used for modeling plant growth and development. Key differences from shape grammars:

1. **Sequential vs. Spatial**: L-systems operate on strings, while shape grammars operate directly on geometric shapes
2. **Parallelism**: L-systems typically apply all rules simultaneously, while shape grammars apply rules sequentially
3. **Interpretation**: L-systems require an interpretation step to convert strings to geometric structures

The plant growth example in VORM uses concepts similar to L-systems but implemented as a shape grammar.

## References and Further Reading

For those interested in learning more about shape grammars:

1. Stiny, G. (1980). "Introduction to shape and shape grammars." Environment and Planning B, 7(3), 343-351.
2. Knight, T. (1999). "Shape grammars in education and practice: History and prospects." International Journal of Design Computing, 2.
3. Duarte, J. P. (2005). "Towards the mass customization of housing: The grammar of Siza's houses at Malagueira." Environment and Planning B: Planning and Design, 32(3), 347-380.
4. Stiny, G. (2006). "Shape: Talking about Seeing and Doing." MIT Press.

## Implementation in VORM

The VORM system implements shape grammars using:

1. **Object-oriented shape representation**: Classes for different shape types
2. **Transformation operations**: Translation, rotation, scaling, reflection
3. **Pattern matching algorithm**: Comparing shapes and establishing variable bindings
4. **Rule application mechanism**: Applying transformations based on matched patterns
5. **S-expression parser**: Converting textual descriptions to runnable grammars
