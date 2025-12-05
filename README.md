# Expressions - Pharo Kata

A mathematical expression interpreter that builds and evaluates expression trees like `(3 + 4) * 5`.

## Goal

Build an interpreter for mathematical expressions using object-oriented design. Expressions form trees that can be evaluated, printed, and manipulated polymorphically.

```smalltalk
| expr |
expr := EAddition new
    left: (EConstant new value: 3);
    right: (EMultiplication new
        left: (EConstant new value: 4);
        right: (EConstant new value: 5)).
expr evaluate.  "Returns 23"
```

## CRC Cards

### EExpression

```
┌─────────────────────────────────────────────────────────┐
│ EExpression                                             │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Define common interface for   │ ENegation               │
│   all expressions             │                         │
│ Provide negated method        │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EConstant

```
┌─────────────────────────────────────────────────────────┐
│ EConstant                                               │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Hold a numeric value          │                         │
│ Return value when evaluated   │                         │
└───────────────────────────────┴─────────────────────────┘
```

### ENegation

```
┌─────────────────────────────────────────────────────────┐
│ ENegation                                               │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Wrap an expression            │ EExpression (any)       │
│ Negate the evaluated result   │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EAddition

```
┌─────────────────────────────────────────────────────────┐
│ EAddition                                               │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Hold left and right operands  │ EExpression (left)      │
│ Return sum when evaluated     │ EExpression (right)     │
└───────────────────────────────┴─────────────────────────┘
```

### EMultiplication

```
┌─────────────────────────────────────────────────────────┐
│ EMultiplication                                         │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Hold left and right operands  │ EExpression (left)      │
│ Return product when evaluated │ EExpression (right)     │
└───────────────────────────────┴─────────────────────────┘
```

## Sequence Diagrams

### Evaluating a Constant

```
┌──────┐          ┌──────────┐
│Client│          │EConstant │
└──┬───┘          └────┬─────┘
   │                   │
   │  value: 5         │
   │──────────────────>│
   │                   │
   │  evaluate         │
   │──────────────────>│
   │                   │
   │       5           │
   │<──────────────────│
   │                   │
```

### Evaluating a Negation

```
┌──────┐       ┌──────────┐       ┌──────────┐
│Client│       │ENegation │       │EConstant │
└──┬───┘       └────┬─────┘       └────┬─────┘
   │                │                  │
   │  evaluate      │                  │
   │───────────────>│                  │
   │                │                  │
   │                │  evaluate        │
   │                │─────────────────>│
   │                │                  │
   │                │       5          │
   │                │<─────────────────│
   │                │                  │
   │                │  5 negated = -5  │
   │                ├───┐              │
   │                │   │              │
   │                │<──┘              │
   │      -5        │                  │
   │<───────────────│                  │
   │                │                  │
```

### Evaluating an Addition (5 + 3)

```
┌──────┐       ┌──────────┐       ┌──────────┐       ┌──────────┐
│Client│       │EAddition │       │EConstant │       │EConstant │
│      │       │          │       │ (left=5) │       │(right=3) │
└──┬───┘       └────┬─────┘       └────┬─────┘       └────┬─────┘
   │                │                  │                  │
   │  evaluate      │                  │                  │
   │───────────────>│                  │                  │
   │                │                  │                  │
   │                │  evaluate        │                  │
   │                │─────────────────>│                  │
   │                │                  │                  │
   │                │       5          │                  │
   │                │<─────────────────│                  │
   │                │                  │                  │
   │                │  evaluate        │                  │
   │                │─────────────────────────────────────>│
   │                │                  │                  │
   │                │       3          │                  │
   │                │<─────────────────────────────────────│
   │                │                  │                  │
   │                │  5 + 3 = 8       │                  │
   │                ├───┐              │                  │
   │                │   │              │                  │
   │                │<──┘              │                  │
   │       8        │                  │                  │
   │<───────────────│                  │                  │
   │                │                  │                  │
```

### Using negated Message

```
┌──────┐       ┌──────────┐       ┌──────────┐
│Client│       │EConstant │       │ENegation │
└──┬───┘       └────┬─────┘       └────┬─────┘
   │                │                  │
   │  value: 5      │                  │
   │───────────────>│                  │
   │                │                  │
   │  negated       │                  │
   │───────────────>│                  │
   │                │                  │
   │                │  ENegation new   │
   │                │  expression: self│
   │                │─────────────────>│
   │                │                  │
   │                │    anENegation   │
   │                │<─────────────────│
   │                │                  │
   │  anENegation   │                  │
   │<───────────────│                  │
   │                │                  │
```

## Class Hierarchy

```
EExpression (abstract)
├── EConstant          - Holds a numeric value
├── ENegation          - Negates an expression
├── EAddition          - Adds two expressions
└── EMultiplication    - Multiplies two expressions
```

## Expression Tree Diagram

The expression `-5 + 3` creates this tree:

```
        anEAddition
        /         \
    left          right
      |             |
 anENegation   anEConstant
      |             |
 expression       value
      |             |
 anEConstant        3
      |
    value
      |
      5
```

## Design Patterns

### Composite Pattern
Expressions form a tree where:
- Leaves: `EConstant`
- Composites: `ENegation`, `EAddition`, `EMultiplication`

All respond to `evaluate` polymorphically.

### Template Method Pattern (future)
When `printOn:` is added, `EBinaryExpression` will use a template that calls `operatorString` hook.

## Progress

- [x] Step 1: EConstant with evaluate
- [x] Step 2: ENegation
- [x] Step 3: EAddition
- [x] Step 4: EMultiplication
- [x] Step 5: Introduce EExpression superclass
- [x] Step 6: Add negated message to EConstant
- [x] Step 7: Factor negated to EExpression
- [ ] Step 8: Class creation methods (value:, left:right:, etc.)
- [ ] Step 9: Example class methods (<sampleInstance>)
- [ ] Step 10: printOn: for all expressions
- [ ] Step 11: Optimize negated for ENegation
- [ ] Step 12: Introduce EBinaryExpression
- [ ] Step 13: Template/hook for operatorString
- [ ] Step 14: EVariable with evaluateWith:

## Key Concepts

### Message vs Method
- **Message**: An intent (what should be done) - e.g., `evaluate`
- **Method**: Implementation (how it's done) - different per class

Sending `evaluate` to different expressions invokes different methods.

### Method Lookup
1. Start in receiver's class
2. If not found, look in superclass
3. Continue up inheritance chain

### Self-sends Create Hooks
Every `self messageName` in a method creates a hook that subclasses can override.

## Tests

### EConstantTest
- `testEvaluate` - Constant returns its value
- `testNegated` - Negating returns ENegation

### ENegationTest
- `testEvaluate` - Returns negated value

### EAdditionTest
- `testEvaluate` - Returns sum

### EMultiplicationTest
- `testEvaluate` - Returns product

## Usage Examples

```smalltalk
"Simple constant"
(EConstant new value: 5) evaluate.  "Returns 5"

"Negation"
(EConstant new value: 5) negated evaluate.  "Returns -5"

"Addition"
| left right |
left := EConstant new value: 3.
right := EConstant new value: 5.
(EAddition new left: left; right: right) evaluate.  "Returns 8"

"Multiplication"
| left right |
left := EConstant new value: 3.
right := EConstant new value: 5.
(EMultiplication new left: left; right: right) evaluate.  "Returns 15"

"Complex expression: (3 + 4) * 5"
| add mult |
add := EAddition new
    left: (EConstant new value: 3);
    right: (EConstant new value: 4).
mult := EMultiplication new
    left: add;
    right: (EConstant new value: 5).
mult evaluate.  "Returns 35"
```

## Source Code

### Package: Expressions

#### Class: EExpression

```smalltalk
Object subclass: #EExpression
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I am the abstract superclass for all expression types in the expression interpreter.

My subclasses represent different kinds of mathematical expressions that can be evaluated to produce a numeric result.

All expressions respond to:
- evaluate - returns the numeric value of this expression
- negated - returns a new ENegation wrapping this expression

Subclasses: EConstant, ENegation, EAddition, EMultiplication
```

**Instance Methods:**

```smalltalk
negated
    ^ ENegation new expression: self
```

---

#### Class: EConstant

```smalltalk
EExpression subclass: #EConstant
    instanceVariableNames: 'value'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent a constant numeric value in an expression tree.

I am a leaf node - I have no sub-expressions.

Example:
    (EConstant new value: 5) evaluate  "Returns 5"

Instance Variables:
    value - the numeric value I hold
```

**Instance Methods:**

```smalltalk
evaluate
    ^ value

value: anInteger
    value := anInteger
```

---

#### Class: ENegation

```smalltalk
EExpression subclass: #ENegation
    instanceVariableNames: 'expression'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent the negation of an expression.

I wrap another expression and return its negated value when evaluated.

Example:
    (ENegation new expression: (EConstant new value: 5)) evaluate  "Returns -5"
    (EConstant new value: 5) negated evaluate  "Same thing, nicer syntax"

Instance Variables:
    expression - the expression to negate
```

**Instance Methods:**

```smalltalk
expression: anExpression
    expression := anExpression

evaluate
    ^ expression evaluate negated
```

---

#### Class: EAddition

```smalltalk
EExpression subclass: #EAddition
    instanceVariableNames: 'left right'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent the addition of two expressions.

I evaluate both sub-expressions and return their sum.

Example:
    | left right |
    left := EConstant new value: 3.
    right := EConstant new value: 5.
    (EAddition new left: left; right: right) evaluate  "Returns 8"

Instance Variables:
    left - the left operand expression
    right - the right operand expression
```

**Instance Methods:**

```smalltalk
left: anExpression
    left := anExpression

right: anExpression
    right := anExpression

evaluate
    ^ left evaluate + right evaluate
```

---

#### Class: EMultiplication

```smalltalk
EExpression subclass: #EMultiplication
    instanceVariableNames: 'left right'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent the multiplication of two expressions.

I evaluate both sub-expressions and return their product.

Example:
    | left right |
    left := EConstant new value: 3.
    right := EConstant new value: 5.
    (EMultiplication new left: left; right: right) evaluate  "Returns 15"

Instance Variables:
    left - the left operand expression
    right - the right operand expression
```

**Instance Methods:**

```smalltalk
left: anExpression
    left := anExpression

right: anExpression
    right := anExpression

evaluate
    ^ left evaluate * right evaluate
```

---

### Package: Expressions-Tests

#### Class: EConstantTest

```smalltalk
TestCase subclass: #EConstantTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions-Tests'
```

**Test Methods:**

```smalltalk
testEvaluate
    self assert: (EConstant new value: 5) evaluate equals: 5

testNegated
    self assert: (EConstant new value: 6) negated evaluate equals: -6
```

---

#### Class: ENegationTest

```smalltalk
TestCase subclass: #ENegationTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions-Tests'
```

**Test Methods:**

```smalltalk
testEvaluate
    self assert: (ENegation new expression: (EConstant new value: 5)) evaluate equals: -5
```

---

#### Class: EAdditionTest

```smalltalk
TestCase subclass: #EAdditionTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions-Tests'
```

**Test Methods:**

```smalltalk
testEvaluate
    | ep1 ep2 |
    ep1 := EConstant new value: 5.
    ep2 := EConstant new value: 3.
    self assert: (EAddition new right: ep1; left: ep2) evaluate equals: 8
```

---

#### Class: EMultiplicationTest

```smalltalk
TestCase subclass: #EMultiplicationTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions-Tests'
```

**Test Methods:**

```smalltalk
testEvaluate
    | ep1 ep2 |
    ep1 := EConstant new value: 5.
    ep2 := EConstant new value: 3.
    self assert: (EMultiplication new right: ep1; left: ep2) evaluate equals: 15
```
 institution 
