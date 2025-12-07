Class {
	#name : 'EEvaluatorVistorTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EEvaluatorVistorTest >> testVisitAdditionReturnsAdditionResult [
	| expression result |
	expression := EAddition
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: 5
]

{ #category : 'tests' }
EEvaluatorVistorTest >> testVisitConstantReturnsConstantValue [
	| constant result |
	constant := EConstant value: 5.
	result := constant accept: EEvaluatorVisitor new.
	self assert: result equals: 5
]
