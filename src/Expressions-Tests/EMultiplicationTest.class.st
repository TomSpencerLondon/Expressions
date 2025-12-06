Class {
	#name : 'EMultiplicationTest',
	#superclass : 'TestCase',
	#instVars : [
		'left'
	],
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EMultiplicationTest >> testClassCreation [
	self assert: (EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 15

]

{ #category : 'tests' }
EMultiplicationTest >> testEvaluate [
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: (EMultiplication new right: ep1; left: ep2) evaluate equals: 15
]

{ #category : 'tests' }
EMultiplicationTest >> testEvaluateWith [
	| expr bindings |
	expr := EMultiplication
		left: (EVariable named: 'x')
		right: (EConstant value: 3).
	bindings := Dictionary newFrom: { 'x' -> 5 }.
	self assert: (expr evaluateWith: bindings) equals: 15
]

{ #category : 'tests' }
EMultiplicationTest >> testPrintOn [
	self assert: (EMultiplication left: (EConstant value: 3) right: (EConstant value: 5)) printString equals: '(3 * 5)'
]
