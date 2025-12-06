Class {
	#name : 'EVariableTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EVariableTest >> testEvaluateWith [
	| expr bindings |
	expr := EVariable named: 'x'.
	bindings := Dictionary newFrom: { 'x' -> 5 }.
	self assert: (expr evaluateWith: bindings) equals: 5
]

{ #category : 'tests' }
EVariableTest >> testEvaluateWithInAddition [
	| expr bindings |
	expr := EAddition
		left: (EVariable named: 'x')
		right: (EConstant value: 3).
	bindings := Dictionary newFrom: { 'x' -> 5 }.
	self assert: (expr evaluateWith: bindings) equals: 8
]

{ #category : 'tests' }
EVariableTest >> testEvaluateWithTwoVariables [
	| expr bindings |
	expr := EMultiplication
		left: (EVariable named: 'x')
		right: (EVariable named: 'y').
	bindings := Dictionary newFrom: { 'x' -> 3. 'y' -> 4 }.
	self assert: (expr evaluateWith: bindings) equals: 12
]

{ #category : 'tests' }
EVariableTest >> testPrintOn [
	self assert: (EVariable named: 'x') printString equals: 'x'
]

{ #category : 'tests' }
EVariableTest >> testSampleInstance [
	self assert: EVariable sampleInstance printString equals: 'x'
]
