Class {
	#name : 'EEvaluatorVistorTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EEvaluatorVistorTest >> testVisitConstantReturnsConstantValue [
	| constant result |
	constant := EConstant value: 5.
	result := constant accept: EEvaluatorVisitor new.
	self assert: result equals: 5
]
