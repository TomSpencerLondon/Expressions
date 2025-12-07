# Expressions - Pharo Kata

A mathematical expression interpreter that builds and evaluates expression trees like `(3 + 4) * 5`.

## Goal

Build an interpreter for mathematical expressions using object-oriented design. Expressions form trees that can be evaluated, printed, and manipulated polymorphically.

```smalltalk
| expr |
expr := EAddition
    left: (EConstant value: 3)
    right: (EMultiplication
        left: (EConstant value: 4)
        right: (EConstant value: 5)).
expr evaluate.  "Returns 23"
expr printString.  "Returns '(3 + (4 * 5))'"
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

### EBinaryExpression

```
┌─────────────────────────────────────────────────────────┐
│ EBinaryExpression                                       │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Hold left and right operands  │ EExpression (left)      │
│ Provide common printing       │ EExpression (right)     │
│ Define operatorString hook    │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EAddition

```
┌─────────────────────────────────────────────────────────┐
│ EAddition                                               │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Return sum when evaluated     │ EBinaryExpression       │
│ Provide ' + ' operator string │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EMultiplication

```
┌─────────────────────────────────────────────────────────┐
│ EMultiplication                                         │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Return product when evaluated │ EBinaryExpression       │
│ Provide ' * ' operator string │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EVariable

```
┌─────────────────────────────────────────────────────────┐
│ EVariable                                               │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Hold a variable name          │ Dictionary (bindings)   │
│ Look up value in bindings     │                         │
└───────────────────────────────┴─────────────────────────┘
```

## Sequence Diagrams

### Evaluating a Constant

```
┌──────┐          ┌─────────��┐
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

### Evaluating a Variable with Bindings

```
┌──────┐       ┌──────────┐       ┌────────────┐
│Client│       │EVariable │       │ Dictionary │
└──┬───┘       └────┬─────┘       └─────┬──────┘
   │                │                   │
   │ evaluateWith:  │                   │
   │ {'x'->5}       │                   │
   │───────────────>│                   │
   │                │                   │
   │                │  at: 'x'          │
   │                │──────────────────>│
   │                │                   │
   │                │       5           │
   │                │<──────────────────│
   │                │                   │
   │       5        │                   │
   │<───────────────│                   │
   │                │                   │
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
├── EVariable          - Holds a variable name, evaluated with bindings
└── EBinaryExpression (abstract) - Base for binary operations
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
- Leaves: `EConstant`, `EVariable`
- Composites: `ENegation`, `EAddition`, `EMultiplication`

All respond to `evaluate` and `evaluateWith:` polymorphically.

### Template Method Pattern
`EBinaryExpression` uses a template method for `printOn:` that calls the `operatorString` hook. Subclasses override `operatorString` to provide their operator symbol.

```smalltalk
"Template method in EBinaryExpression"
printOn: aStream
    aStream nextPut: $(.
    left printOn: aStream.
    aStream nextPutAll: self operatorString.  "Hook method"
    right printOn: aStream.
    aStream nextPut: $)

"Hook implementations"
EAddition>>operatorString
    ^ ' + '

EMultiplication>>operatorString
    ^ ' * '
```

## Progress

- [x] Step 1: EConstant with evaluate
- [x] Step 2: ENegation
- [x] Step 3: EAddition
- [x] Step 4: EMultiplication
- [x] Step 5: Introduce EExpression superclass
- [x] Step 6: Add negated message to EConstant
- [x] Step 7: Factor negated to EExpression
- [x] Step 8: Class creation methods (value:, left:right:, etc.)
- [x] Step 9: Example class methods (sampleInstance)
- [x] Step 10: printOn: for all expressions
- [x] Step 11: Optimize negated for ENegation
- [x] Step 12: Introduce EBinaryExpression
- [x] Step 13: Template/hook for operatorString
- [x] Step 14: EVariable with evaluateWith:

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

### Subclass Responsibility
Abstract methods use `self subclassResponsibility` to indicate subclasses must implement them.

## Tests

### EConstantTest
- `testEvaluate` - Constant returns its value
- `testClassCreation` - Class-side value: works
- `testNegated` - Negating returns ENegation
- `testPrintOn` - Prints the value

### ENegationTest
- `testEvaluate` - Returns negated value
- `testClassCreation` - Class-side expression: works
- `testNegatedNegated` - Double negation returns original
- `testPrintOn` - Prints as -(expr)
- `testEvaluateWith` - Works with variable bindings

### EAdditionTest
- `testEvaluate` - Returns sum
- `testClassCreation` - Class-side left:right: works
- `testPrintOn` - Prints as (left + right)

### EMultiplicationTest
- `testEvaluate` - Returns product
- `testClassCreation` - Class-side left:right: works
- `testPrintOn` - Prints as (left * right)
- `testEvaluateWith` - Works with variable bindings

### EVariableTest
- `testEvaluateWith` - Returns value from bindings
- `testPrintOn` - Prints the variable name
- `testEvaluateWithInAddition` - Variables work in expressions
- `testEvaluateWithTwoVariables` - Multiple variables work

## Usage Examples

```smalltalk
"Simple constant"
(EConstant value: 5) evaluate.  "Returns 5"

"Negation"
(EConstant value: 5) negated evaluate.  "Returns -5"

"Addition"
(EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate.  "Returns 8"

"Multiplication"
(EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) evaluate.  "Returns 15"

"Complex expression: (3 + 4) * 5"
| add mult |
add := EAddition left: (EConstant value: 3) right: (EConstant value: 4).
mult := EMultiplication left: add right: (EConstant value: 5).
mult evaluate.  "Returns 35"
mult printString.  "Returns '((3 + 4) * 5)'"

"Variables: x + 3 where x = 5"
| expr bindings |
expr := EAddition left: (EVariable named: 'x') right: (EConstant value: 3).
bindings := Dictionary newFrom: { 'x' -> 5 }.
expr evaluateWith: bindings.  "Returns 8"
expr printString.  "Returns '(x + 3)'"

"Multiple variables: x * y where x = 3, y = 4"
| expr bindings |
expr := EMultiplication left: (EVariable named: 'x') right: (EVariable named: 'y').
bindings := Dictionary newFrom: { 'x' -> 3. 'y' -> 4 }.
expr evaluateWith: bindings.  "Returns 12"
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
- evaluateWith: - returns the numeric value using variable bindings
- negated - returns a new ENegation wrapping this expression

Subclasses: EConstant, ENegation, EBinaryExpression, EVariable
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
    (EConstant value: 5) evaluate  "Returns 5"

Instance Variables:
    value - the numeric value I hold
```

**Class Methods:**

```smalltalk
value: anInteger
    ^ self new value: anInteger

sampleInstance
    <sampleInstance>
    ^ self value: 5
```

**Instance Methods:**

```smalltalk
value: anInteger
    value := anInteger

evaluate
    ^ value

evaluateWith: aDictionary
    ^ value

negated
    ^ ENegation new expression: self

printOn: aStream
    aStream print: value
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
    (ENegation expression: (EConstant value: 5)) evaluate  "Returns -5"
    (EConstant value: 5) negated evaluate  "Same thing, nicer syntax"

Instance Variables:
    expression - the expression to negate
```

**Class Methods:**

```smalltalk
expression: anExpression
    ^ self new expression: anExpression

sampleInstance
    <sampleInstance>
    ^ self expression: (EConstant value: 5)
```

**Instance Methods:**

```smalltalk
expression: anExpression
    expression := anExpression

evaluate
    ^ expression evaluate negated

evaluateWith: aDictionary
    ^ (expression evaluateWith: aDictionary) negated

negated
    ^ expression

printOn: aStream
    aStream nextPut: $-.
    aStream nextPut: $(.
    expression printOn: aStream.
    aStream nextPut: $)
```

---

#### Class: EBinaryExpression

```smalltalk
EExpression subclass: #EBinaryExpression
    instanceVariableNames: 'left right'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I am the abstract superclass for binary expressions (expressions with two operands).

I factor out the common structure of left and right operands shared by EAddition, EMultiplication, and other binary operations.

Subclasses must implement:
- evaluate - to define how the two operands are combined
- evaluateWith: - to evaluate with variable bindings
- operatorString - to provide the operator symbol for printing

Instance Variables:
    left - the left operand expression
    right - the right operand expression

Example (using a concrete subclass):
    EAddition left: (EConstant value: 3) right: (EConstant value: 5)
```

**Class Methods:**

```smalltalk
left: anExpression right: anotherExpression
    ^ self new left: anExpression; right: anotherExpression
```

**Instance Methods:**

```smalltalk
left: anExpression
    left := anExpression

right: anExpression
    right := anExpression

operatorString
    self subclassResponsibility

printOn: aStream
    aStream nextPut: $(.
    left printOn: aStream.
    aStream nextPutAll: self operatorString.
    right printOn: aStream.
    aStream nextPut: $)
```

---

#### Class: EAddition

```smalltalk
EBinaryExpression subclass: #EAddition
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent the addition of two expressions.

I evaluate both sub-expressions and return their sum.

Example:
    (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate  "Returns 8"
```

**Class Methods:**

```smalltalk
sampleInstance
    <sampleInstance>
    ^ self left: (EConstant value: 3) right: (EConstant value: 5)
```

**Instance Methods:**

```smalltalk
evaluate
    ^ left evaluate + right evaluate

evaluateWith: aDictionary
    ^ (left evaluateWith: aDictionary) + (right evaluateWith: aDictionary)

operatorString
    ^ ' + '
```

---

#### Class: EMultiplication

```smalltalk
EBinaryExpression subclass: #EMultiplication
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent the multiplication of two expressions.

I evaluate both sub-expressions and return their product.

Example:
    (EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) evaluate  "Returns 15"
```

**Class Methods:**

```smalltalk
sampleInstance
    <sampleInstance>
    ^ self left: (EConstant value: 3) right: (EConstant value: 5)
```

**Instance Methods:**

```smalltalk
evaluate
    ^ left evaluate * right evaluate

evaluateWith: aDictionary
    ^ (left evaluateWith: aDictionary) * (right evaluateWith: aDictionary)

operatorString
    ^ ' * '
```

---

#### Class: EVariable

```smalltalk
EExpression subclass: #EVariable
    instanceVariableNames: 'name'
    classVariableNames: ''
    package: 'Expressions'
```

**Class Comment:**
```
I represent a variable in an expression tree.

When evaluated with a dictionary of bindings, I look up my name and return the corresponding value.

Example:
    (EVariable named: 'x') evaluateWith: { 'x' -> 5 } asDictionary  "Returns 5"

Instance Variables:
    name - the variable name as a String
```

**Class Methods:**

```smalltalk
named: aString
    ^ self new name: aString

sampleInstance
    <sampleInstance>
    ^ self named: 'x'
```

**Instance Methods:**

```smalltalk
name: aString
    name := aString

evaluateWith: aDictionary
    ^ aDictionary at: name

printOn: aStream
    aStream nextPutAll: name
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

testClassCreation
    self assert: (EConstant value: 5) evaluate equals: 5

testNegated
    self assert: (EConstant new value: 6) negated evaluate equals: -6

testPrintOn
    self assert: (EConstant value: 5) printString equals: '5'
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

testClassCreation
    self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 8

testNegatedNegated
    | constant |
    constant := EConstant value: 5.
    self assert: constant negated negated equals: constant

testPrintOn
    self assert: (ENegation expression: (EConstant value: 5)) printString equals: '-(5)'

testEvaluateWith
    | expr bindings |
    expr := ENegation expression: (EVariable named: 'x').
    bindings := Dictionary newFrom: { 'x' -> 5 }.
    self assert: (expr evaluateWith: bindings) equals: -5
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

testClassCreation
    self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 8

testPrintOn
    self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) printString equals: '(3 + 5)'
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

testClassCreation
    self assert: (EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 15

testPrintOn
    self assert: (EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) printString equals: '(3 * 5)'

testEvaluateWith
    | expr bindings |
    expr := EMultiplication left: (EVariable named: 'x') right: (EConstant value: 3).
    bindings := Dictionary newFrom: { 'x' -> 5 }.
    self assert: (expr evaluateWith: bindings) equals: 15
```

---

#### Class: EVariableTest

```smalltalk
TestCase subclass: #EVariableTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Expressions-Tests'
```

**Test Methods:**

```smalltalk
testEvaluateWith
    | expr bindings |
    expr := EVariable named: 'x'.
    bindings := Dictionary newFrom: { 'x' -> 5 }.
    self assert: (expr evaluateWith: bindings) equals: 5

testPrintOn
    self assert: (EVariable named: 'x') printString equals: 'x'

testEvaluateWithInAddition
    | expr bindings |
    expr := EAddition left: (EVariable named: 'x') right: (EConstant value: 3).
    bindings := Dictionary newFrom: { 'x' -> 5 }.
    self assert: (expr evaluateWith: bindings) equals: 8

testEvaluateWithTwoVariables
    | expr bindings |
    expr := EMultiplication left: (EVariable named: 'x') right: (EVariable named: 'y').
    bindings := Dictionary newFrom: { 'x' -> 3. 'y' -> 4 }.
    self assert: (expr evaluateWith: bindings) equals: 12
```
