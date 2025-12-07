Class {
	#name : 'EEvaluatorVisitorTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EEvaluatorVisitorTest >> testVisitAdditionReturnsAdditionResult [
	| expression result |
	expression := EAddition
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: 5
]

{ #category : 'tests' }
EEvaluatorVisitorTest >> testVisitConstantReturnsConstantValue [
	| constant result |
	constant := EConstant value: 5.
	result := constant accept: EEvaluatorVisitor new.
	self assert: result equals: 5
]

{ #category : 'tests' }
EEvaluatorVisitorTest >> testVisitMultiplicationReturnsMultiplicationResult [
	| expression result |
	expression := EMultiplication
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: -14
]

{ #category : 'tests' }
EEvaluatorVisitorTest >> testVisitNegationReturnsNegatedConstant [
	| expression result |
	expression := (EConstant value: 7) negated.
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: -7
]

{ #category : 'tests' }
EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue [
	| expression result visitor |
	expression := EVariable named: 'x'.
	visitor := EEvaluatorVisitor new.
	visitor at: 'x' put: 42.
	result := expression accept: visitor.
	self assert: result equals: 42
]
