# Expressions Visitor - Pharo Kata

A refactoring of the Expressions kata to use the **Visitor Pattern**, following Chapter 12 of "Learning OOP with Pharo".

## Starting Point

This kata builds on the completed Expressions kata, which has:
- Expression tree classes: `EExpression`, `EConstant`, `ENegation`, `EBinaryExpression`, `EAddition`, `EMultiplication`, `EVariable`
- Each class has `evaluate`, `evaluateWith:`, `printOn:` methods embedded
- `EVariable` uses `evaluateWith:` with a bindings dictionary

## Goal

Refactor to use the **Visitor Pattern**:
- Extract evaluation behavior into `EEvaluatorVisitor`
- Extract printing behavior into `EPrinterVisitor`
- Add `accept:` method to each expression class (double dispatch)

## Why Visitor Pattern?

| Approach | Add New Expression | Add New Operation |
|----------|-------------------|-------------------|
| **Methods in classes** | Easy - add one class | Hard - touch every class |
| **Visitor pattern** | Hard - touch every visitor | Easy - add one visitor |

**Benefits:**
- Visitor encapsulates its own state (e.g., bindings dictionary)
- Multiple visitors can operate on same structure
- New operations don't require modifying expression classes
- Modular - can load/execute visitors independently

## The Key Pattern: Double Dispatch

```smalltalk
"1. Expression tells visitor HOW to visit it"
EConstant >> accept: aVisitor
    ^ aVisitor visitConstant: self

"2. Visitor defines WHAT to do"
EEvaluatorVisitor >> visitConstant: aConstant
    ^ aConstant value
```

The interaction:
```smalltalk
| constant result |
constant := EConstant value: 5.
result := constant accept: EEvaluatorVisitor new.  "Returns 5"
```

## Class Hierarchy (After Refactoring)

```
EExpression (abstract)
├── accept: aVisitor → self subclassResponsibility
├── EConstant
│   └── accept: aVisitor → aVisitor visitConstant: self
├── ENegation
│   └── accept: aVisitor → aVisitor visitNegation: self
├── EVariable
│   └── accept: aVisitor → aVisitor visitVariable: self
└── EBinaryExpression (abstract)
    ├── EAddition
    │   └── accept: aVisitor → aVisitor visitAddition: self
    └── EMultiplication
        └── accept: aVisitor → aVisitor visitMultiplication: self

EEvaluatorVisitor
├── bindings (instance variable for variable lookup)
├── visitConstant: → aConstant value
├── visitNegation: → (expression accept: self) negated
├── visitAddition: → (left accept: self) + (right accept: self)
├── visitMultiplication: → (left accept: self) * (right accept: self)
└── visitVariable: → bindings at: aVariable name

EPrinterVisitor
├── visitConstant: → aConstant value asString
├── visitNegation: → '-(' , (expression accept: self) , ')'
├── visitAddition: → '(' , left , ' + ' , right , ')'
├── visitMultiplication: → '(' , left , ' * ' , right , ')'
└── visitVariable: → aVariable name
```

## Visitor Pattern Refactoring Progress

- [x] Step 1: Add `accept:` to EExpression and EConstant, create EEvaluatorVisitor
- [x] Step 2: Handle EAddition with `accept:` and `visitAddition:`
- [x] Step 3: Handle ENegation with `accept:` and `visitNegation:`
- [x] Step 4: Handle EMultiplication with `accept:` and `visitMultiplication:`
- [x] Step 5: Handle EVariable with bindings support in visitor
- [x] Step 6: Create EPrinterVisitor
- [x] Step 7: Connect old API (`evaluateWith:`) to new visitor pattern
- [x] Step 8: Cleanup and documentation

## Step-by-Step Implementation

### Step 1: EConstant + EEvaluatorVisitor

**Test first:**
```smalltalk
EEvaluatorVisitorTest >> testVisitConstantReturnsConstantValue
    | constant result |
    constant := EConstant value: 5.
    result := constant accept: EEvaluatorVisitor new.
    self assert: result equals: 5
```

**Implementation:**
```smalltalk
EExpression >> accept: aVisitor
    self subclassResponsibility

EConstant >> accept: aVisitor
    ^ aVisitor visitConstant: self

EEvaluatorVisitor >> visitConstant: aConstant
    ^ aConstant value
```

### Step 2: EAddition

**Test:**
```smalltalk
EEvaluatorVisitorTest >> testVisitAdditionReturnsAdditionResult
    | expression result |
    expression := EAddition
        left: (EConstant value: 7)
        right: (EConstant value: -2).
    result := expression accept: EEvaluatorVisitor new.
    self assert: result equals: 5
```

**Implementation:**
```smalltalk
EAddition >> accept: aVisitor
    ^ aVisitor visitAddition: self

EBinaryExpression >> left
    ^ left

EBinaryExpression >> right
    ^ right

EEvaluatorVisitor >> visitAddition: anAddition
    ^ (anAddition left accept: self) + (anAddition right accept: self)
```

### Step 3: ENegation

**Test:**
```smalltalk
EEvaluatorVisitorTest >> testVisitNegationReturnsNegatedConstant
    | expression result |
    expression := (EConstant value: 7) negated.
    result := expression accept: EEvaluatorVisitor new.
    self assert: result equals: -7
```

**Implementation:**
```smalltalk
ENegation >> accept: aVisitor
    ^ aVisitor visitNegation: self

ENegation >> expression
    ^ expression

EEvaluatorVisitor >> visitNegation: aNegation
    ^ (aNegation expression accept: self) negated
```

### Step 4: EMultiplication

**Test:**
```smalltalk
EEvaluatorVisitorTest >> testVisitMultiplicationReturnsMultiplicationResult
    | expression result |
    expression := EMultiplication
        left: (EConstant value: 7)
        right: (EConstant value: -2).
    result := expression accept: EEvaluatorVisitor new.
    self assert: result equals: -14
```

**Implementation:**
```smalltalk
EMultiplication >> accept: aVisitor
    ^ aVisitor visitMultiplication: self

EEvaluatorVisitor >> visitMultiplication: aMultiplication
    ^ (aMultiplication left accept: self) * (aMultiplication right accept: self)
```

### Step 5: EVariable with Bindings

**Test:**
```smalltalk
EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue
    | expression result visitor |
    expression := EVariable named: 'x'.
    visitor := EEvaluatorVisitor new.
    visitor at: 'x' put: 42.
    result := expression accept: visitor.
    self assert: result equals: 42
```

**Implementation:**
```smalltalk
Object subclass: #EEvaluatorVisitor
    instanceVariableNames: 'bindings'
    classVariableNames: ''
    package: 'Expressions'

EEvaluatorVisitor >> initialize
    super initialize.
    bindings := Dictionary new

EEvaluatorVisitor >> at: aKey put: aValue
    bindings at: aKey put: aValue

EEvaluatorVisitor >> bindings: aDictionary
    bindings := aDictionary

EVariable >> accept: aVisitor
    ^ aVisitor visitVariable: self

EVariable >> name
    ^ name

EEvaluatorVisitor >> visitVariable: aVariable
    ^ bindings at: aVariable name
```

### Step 6: EPrinterVisitor

**Tests:**
```smalltalk
EPrinterVisitorTest >> testVisitConstant
    self assert: ((EConstant value: 5) accept: EPrinterVisitor new) equals: '5'

EPrinterVisitorTest >> testVisitAddition
    | expr |
    expr := EAddition left: (EConstant value: 3) right: (EConstant value: 5).
    self assert: (expr accept: EPrinterVisitor new) equals: '(3 + 5)'
```

**Implementation:**
```smalltalk
EPrinterVisitor >> visitConstant: aConstant
    ^ aConstant value asString

EPrinterVisitor >> visitAddition: anAddition
    ^ '(' , (anAddition left accept: self) , ' + ' , (anAddition right accept: self) , ')'

EPrinterVisitor >> visitNegation: aNegation
    ^ '-(' , (aNegation expression accept: self) , ')'

EPrinterVisitor >> visitMultiplication: aMultiplication
    ^ '(' , (aMultiplication left accept: self) , ' * ' , (aMultiplication right accept: self) , ')'

EPrinterVisitor >> visitVariable: aVariable
    ^ aVariable name
```

### Step 7: Connect Old API to Visitor

**Purpose:** Make the existing `evaluateWith:` method use the visitor pattern internally, so existing code continues to work unchanged.

**Implementation:**
```smalltalk
EExpression >> evaluateWith: aDictionary
    | visitor |
    visitor := EEvaluatorVisitor new.
    visitor bindings: aDictionary.
    ^ self accept: visitor
```

**What this does:**
- Existing code calling `evaluateWith:` continues to work
- Internally, it creates a visitor, sets bindings, and delegates to `accept:`
- This is the "bridge" between the old API and the new visitor pattern

**Sequence Diagram:**
```
┌──────┐     ┌───────────┐     ┌─────────────────┐     ┌──────────┐
│Client│     │EExpression│     │EEvaluatorVisitor│     │EConstant │
└──┬───┘     └─────┬─────┘     └───────┬─────────┘     └────┬─────┘
   │               │                   │                    │
   │ evaluateWith: │                   │                    │
   │ { x -> 5 }    │                   │                    │
   │──────────────>│                   │                    │
   │               │                   │                    │
   │               │ new               │                    │
   │               │──────────────────>│                    │
   │               │                   │                    │
   │               │ bindings: {x->5}  │                    │
   │               │──────────────────>│                    │
   │               │                   │                    │
   │               │ accept: visitor   │                    │
   │               │──────────────────>│                    │
   │               │                   │                    │
   │               │                   │ visitXxx:          │
   │               │                   │───────────────────>│
   │               │                   │                    │
   │               │                   │       result       │
   │               │                   │<───────────────────│
   │               │                   │                    │
   │               │       result      │                    │
   │               │<──────────────────│                    │
   │               │                   │                    │
   │       result  │                   │                    │
   │<──────────────│                   │                    │
```

## CRC Cards

### EEvaluatorVisitor

```
┌─────────────────────────────────────────────────────────┐
│ EEvaluatorVisitor                                       │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Evaluate expression trees     │ EExpression subclasses  │
│ Hold variable bindings        │ Dictionary              │
│ Visit each expression type    │                         │
└───────────────────────────────┴─────────────────────────┘
```

### EPrinterVisitor

```
┌─────────────────────────────────────────────────────────┐
│ EPrinterVisitor                                         │
├─────────────────────────────────────────────────────────┤
│ Responsibilities              │ Collaborators           │
├───────────────────────────────┼─────────────────────────┤
│ Convert expressions to string │ EExpression subclasses  │
│ Visit each expression type    │                         │
└───────────────────────────────┴─────────────────────────┘
```

## Sequence Diagram: Visitor Evaluating Addition

```
┌──────┐     ┌──────────┐     ┌─────────────────┐     ┌──────────┐
│Client│     │EAddition │     │EEvaluatorVisitor│     │EConstant │
└──┬───┘     └────┬─────┘     └───────┬─────────┘     └────┬─────┘
   │              │                   │                    │
   │ accept:      │                   │                    │
   │─────────────>│                   │                    │
   │              │                   │                    │
   │              │ visitAddition:    │                    │
   │              │──────────────────>│                    │
   │              │                   │                    │
   │              │                   │ accept: (left)     │
   │              │                   │───────────────────>│
   │              │                   │                    │
   │              │                   │ visitConstant:     │
   │              │                   │<───────────────────│
   │              │                   │                    │
   │              │                   │       5            │
   │              │                   │<───────────────────│
   │              │                   │                    │
   │              │                   │ accept: (right)    │
   │              │                   │───────────────────>│
   │              │                   │                    │
   │              │                   │       3            │
   │              │                   │<───────────────────│
   │              │                   │                    │
   │              │       8           │                    │
   │              │<──────────────────│                    │
   │              │                   │                    │
   │       8      │                   │                    │
   │<─────────────│                   │                    │
```

## Key Concepts

### Double Dispatch
The visitor pattern uses double dispatch:
1. First dispatch: `expression accept: visitor` - selects based on expression type
2. Second dispatch: `visitor visitXxx: expression` - selects based on visitor type

This avoids type checking (no `isKindOf:` or case statements).

### Visitor State
Unlike methods embedded in expressions, visitors can hold their own state:
- `EEvaluatorVisitor` holds `bindings` dictionary for variable lookup
- This state is separate from the expression tree structure

### Trade-offs
**Use Visitor when:**
- Structure is stable (few new expression types)
- Many operations needed on the structure
- Operations need their own state

**Don't use Visitor when:**
- Structure changes frequently
- Only a few simple operations needed
