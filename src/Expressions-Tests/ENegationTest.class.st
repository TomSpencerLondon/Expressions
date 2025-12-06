Class {
	#name : 'ENegationTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
ENegationTest >> testClassCreation [
	self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 8
]

{ #category : 'tests' }
ENegationTest >> testEvaluate [
	self assert: (ENegation new expression: (EConstant new value: 5)) evaluate equals: -5
]

{ #category : 'tests' }
ENegationTest >> testEvaluateWith [
	| expr bindings |
	expr := ENegation expression: (EVariable named: 'x').
	bindings := Dictionary newFrom: { 'x' -> 5 }.
	self assert: (expr evaluateWith: bindings) equals: -5
]

{ #category : 'tests' }
ENegationTest >> testNegatedNegated [
	| constant |
	constant := EConstant value: 5.
	self assert: constant negated negated equals: constant
]

{ #category : 'tests' }
ENegationTest >> testPrintOn [
	self assert: (ENegation expression: (EConstant value: 5)) printString equals: '-(5)'
]
