# Die For Role Playing - Pharo Kata

A simple domain-specific language (DSL) for rolling dice, as used in role-playing games like Dungeons & Dragons.

## Goal

Build a DSL that allows expressions like:

```smalltalk
(2 D20 + 1 D6) roll
```

This means: "Roll two 20-sided dice and one 6-sided die, then sum the results."

## Classes

### Die

A single die with a configurable number of faces.

| Method | Description |
|--------|-------------|
| `Die new` | Creates a die with 6 faces (default) |
| `Die withFaces: n` | Creates a die with n faces |
| `die faces` | Returns the number of faces |
| `die roll` | Returns a random number between 1 and faces |
| `die printOn: aStream` | Outputs 'a Die (n)' for debugging |

### DieHandle

A collection of dice that can be rolled together.

| Method | Description |
|--------|-------------|
| `DieHandle new` | Creates an empty handle |
| `handle addDie: aDie` | Adds a die to the handle |
| `handle diceNumber` | Returns the count of dice |
| `handle roll` | Rolls all dice and returns the sum |
| `handle dice` | Returns the collection of dice |
| `handle + anotherHandle` | Combines two handles into a new one |

### Integer Extension (*Dice protocol)

| Method | Description |
|--------|-------------|
| `n D: faces` | Creates a DieHandle with n dice of given faces |
| `n D4` | Creates a DieHandle with n 4-sided dice |
| `n D6` | Creates a DieHandle with n 6-sided dice |
| `n D10` | Creates a DieHandle with n 10-sided dice |
| `n D20` | Creates a DieHandle with n 20-sided dice |

## Sequence Diagrams

### Rolling a Single Die

```
┌──────┐          ┌─────┐
│Client│          │ Die │
└──┬───┘          └──┬──┘
   │                 │
   │  Die new        │
   │────────────────>│
   │                 │
   │   aDie          │
   │<────────────────│
   │                 │
   │  roll           │
   │────────────────>│
   │                 │
   │  faces atRandom │
   │                 ├───┐
   │                 │   │ (e.g., 4)
   │                 │<──┘
   │       4         │
   │<────────────────│
   │                 │
```

### Rolling a DieHandle

```
┌──────┐       ┌──────────┐       ┌───────┐       ┌───────┐
│Client│       │DieHandle │       │Die (6)│       │Die(10)│
└──┬───┘       └────┬─────┘       └───┬───┘       └───┬───┘
   │                │                 │               │
   │  roll          │                 │               │
   │───────────────>│                 │               │
   │                │                 │               │
   │                │  roll           │               │
   │                │────────────────>│               │
   │                │                 │               │
   │                │       3         │               │
   │                │<────────────────│               │
   │                │                 │               │
   │                │  roll           │               │
   │                │────────────────────────────────>│
   │                │                 │               │
   │                │       7         │               │
   │                │<────────────────────────────────│
   │                │                 │               │
   │                │  sum: 3 + 7 = 10│               │
   │                ├───┐             │               │
   │                │   │             │               │
   │                │<──┘             │               │
   │      10        │                 │               │
   │<───────────────│                 │               │
   │                │                 │               │
```

### Using the DSL (2 D: 6)

```
┌──────┐       ┌─────────┐       ┌──────────┐       ┌─────┐
│Client│       │ Integer │       │DieHandle │       │ Die │
└──┬───┘       └────┬────┘       └────┬─────┘       └──┬──┘
   │                │                 │                │
   │   2 D: 6       │                 │                │
   │───────────────>│                 │                │
   │                │                 │                │
   │                │ DieHandle new   │                │
   │                │────────────────>│                │
   │                │                 │                │
   │                │   handle        │                │
   │                │<────────────────│                │
   │                │                 │                │
   │                │ 2 timesRepeat: [...]             │
   │                ├───┐             │                │
   │                │   │ Die withFaces: 6             │
   │                │   │────────────────────────────>│
   │                │   │             │                │
   │                │   │           aDie               │
   │                │   │<────────────────────────────│
   │                │   │             │                │
   │                │   │ addDie: aDie│                │
   │                │   │────────────>│                │
   │                │   │             │                │
   │                │   │ (repeat)    │                │
   │                │<──┘             │                │
   │                │                 │                │
   │    handle      │                 │                │
   │<───────────────│                 │                │
   │                │                 │                │
```

## Design Notes

### Polymorphic API

Both `Die` and `DieHandle` respond to `roll`. This means clients don't need to know which type they're dealing with:

```
       ┌─────────────┐
       │   Client    │
       └──────┬──────┘
              │
              │ roll
              ▼
       ┌──────────────┐
       │  «protocol»  │
       │   Rollable   │
       │──────────────│
       │ roll → Number│
       └──────────────┘
              △
              │
     ┌────────┴────────┐
     │                 │
┌────┴────┐      ┌─────┴─────┐
│   Die   │      │ DieHandle │
│─────────│      │───────────│
│ faces   │      │ dice      │
│─────────│      │───────────│
│ roll    │      │ roll      │
└─────────┘      └───────────┘
```

This supports the **"Don't ask, tell"** principle - clients just send `roll` without checking the object type.

### Extending Core Classes

The `D:` method is added to `Integer` using an **extension protocol** (`*Dice`). This keeps the method in the Dice package while extending a Kernel class:

```
┌─────────────────────────────────┐
│ Kernel package                  │
│   Integer class                 │
│   └── (core methods)            │
└─────────────────────────────────┘

┌─────────────────────────────────┐
│ Dice package                    │
│   Die class                     │
│   DieHandle class               │
│   Integer (extension)           │
│   └── protocol: *Dice           │
│       └── D:                    │
└─────────────────────────────────┘
```

## Progress

- [x] Step 1: Die with default faces
- [x] Step 2: Rolling a die
- [x] Step 3: Die with custom faces (withFaces:)
- [x] Step 4: DieHandle creation and addDie:
- [x] Step 5: Rolling a DieHandle
- [x] Step 6: printOn: for better debugging
- [x] Step 7: DSL - Integer >> D:
- [x] Step 8: DSL - D4, D6, D10, D20
- [x] Step 9: DieHandle addition (+)

**KATA COMPLETE!**

## Usage Examples

```smalltalk
"Create and roll a single die"
Die new roll.                    "Returns 1-6"
(Die withFaces: 20) roll.        "Returns 1-20"

"Inspect a die"
(Die withFaces: 6) printString.  "Returns 'a Die (6)'"

"Create and roll a handle of dice (manual way)"
| handle |
handle := DieHandle new.
handle addDie: (Die withFaces: 6).
handle addDie: (Die withFaces: 10).
handle roll.                     "Returns 2-16"

"Create and roll using DSL"
(2 D: 6) roll.                   "Roll 2d6 - returns 2-12"
(3 D: 20) roll.                  "Roll 3d20 - returns 3-60"
(2 D: 6) diceNumber.             "Returns 2"

"Shorthand DSL"
(2 D20) roll.                    "Roll 2d20 - returns 2-40"
(1 D6) roll.                     "Roll 1d6 - returns 1-6"

"Combining handles"
(2 D20 + 1 D6) roll.             "Roll 2d20 + 1d6 - returns 3-46"
(2 D20 + 1 D6) diceNumber.       "Returns 3"
```

## Tests

Run all tests in the `Dice-Tests` package:

### DieTest
- `testInitializeIsOk` - Default die has 6 faces
- `testRolling` - Roll returns value between 1 and faces
- `testPrintOn` - Die prints as 'a Die (n)'

### DieHandleTest
- `testCreationAdding` - Can add dice to handle
- `testAddingTwiceTheSameDice` - Can add same die twice
- `testRoll` - Handle roll sums all dice
- `testDSL` - `2 D: 6` creates handle with 2 dice
- `testD20` - `2 D20` creates handle with 2 dice
- `testAddition` - `(2 D20) + (1 D6)` creates handle with 3 dice

## Source Code

### Package: Dice

#### Class: Die

**Instance Methods:**

```smalltalk
faces
    ^ faces

faces: aNumber
    faces := aNumber

initialize
    faces := 6

printOn: aStream
    super printOn: aStream.
    aStream
        nextPutAll: ' (';
        print: faces;
        nextPutAll: ')'

roll
    ^ faces atRandom
```

**Class Methods:**

```smalltalk
withFaces: aNumber
    | instance |
    instance := self new.
    instance faces: aNumber.
    ^ instance
```

#### Class: DieHandle

**Instance Methods:**

```smalltalk
addDie: aDie
    dice add: aDie

dice
    ^ dice

diceNumber
    ^ dice size

initialize
    dice := OrderedCollection new

roll
    ^ dice sum: [ :each | each roll ]

+ aDieHandle
    | newHandle |
    newHandle := DieHandle new.
    dice do: [ :each | newHandle addDie: each ].
    aDieHandle dice do: [ :each | newHandle addDie: each ].
    ^ newHandle
```

#### Integer Extension (*Dice protocol)

```smalltalk
D: aNumber
    | handle |
    handle := DieHandle new.
    self timesRepeat: [ handle addDie: (Die withFaces: aNumber) ].
    ^ handle

D4
    ^ self D: 4

D6
    ^ self D: 6

D10
    ^ self D: 10

D20
    ^ self D: 20
```

### Package: Dice-Tests

#### Class: DieTest

```smalltalk
testInitializeIsOk
    self assert: Die new faces equals: 6

testPrintOn
    self assert: (Die withFaces: 6) printString equals: 'a Die (6)'

testRolling
    | d |
    d := Die new.
    10 timesRepeat: [ self assert: (d roll between: 1 and: 6) ]
```

#### Class: DieHandleTest

```smalltalk
testAddingTwiceTheSameDice
    | handle |
    handle := DieHandle new.
    handle addDie: (Die withFaces: 6).
    self assert: handle diceNumber equals: 1.
    handle addDie: (Die withFaces: 6).
    self assert: handle diceNumber equals: 2

testCreationAdding
    | handle |
    handle := DieHandle new
        addDie: (Die withFaces: 6);
        addDie: (Die withFaces: 10);
        yourself.
    self assert: handle diceNumber equals: 2

testRoll
    | handle |
    handle := DieHandle new
        addDie: (Die withFaces: 6);
        addDie: (Die withFaces: 10);
        yourself.
    10 timesRepeat: [ self assert: (handle roll between: 2 and: 16) ]

testDSL
    | handle |
    handle := 2 D: 6.
    self assert: handle diceNumber equals: 2

testD20
    | handle |
    handle := 2 D20.
    self assert: handle diceNumber equals: 2

testAddition
    | handle |
    handle := (2 D20) + (1 D6).
    self assert: handle diceNumber equals: 3
```
