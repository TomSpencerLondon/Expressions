"
I am the abstract superclass for all expression types in the expression interpreter.

My subclasses represent different kinds of mathematical expressions that can be evaluated to produce a numeric result.

All expressions respond to:
- evaluate - returns the numeric value of this expression
- negated - returns a new ENegation wrapping this expression

Subclasses: EConstant, ENegation, EAddition, EMultiplication
"
Class {
	#name : 'EExpression',
	#superclass : 'Object',
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'accessing' }
EExpression >> accept: aVisitor [
	self subclassResponsibility
]

{ #category : 'accessing' }
EExpression >> evaluateWith: aDictionary [
	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: aDictionary.
	^ self accept: visitor
]
