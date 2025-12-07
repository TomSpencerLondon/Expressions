"
I am a visitor that evaluates expression trees to produce numeric results.

I implement the Visitor pattern - expressions accept me via accept:, and I define visitXxx: methods for each expression type.

I can hold variable bindings in my bindings dictionary, allowing evaluation of expressions containing EVariable nodes.

Example:
    | expr result |
    expr := EAddition left: (EConstant value: 3) right: (EConstant value: 5).
    result := expr accept: EEvaluatorVisitor new.  ""Returns 8""
"
Class {
	#name : 'EEvaluatorVisitor',
	#superclass : 'Object',
	#instVars : [
		'bindings'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'visiting' }
EEvaluatorVisitor >> accept: aVisitor [
	self subclassResponsibility
]

{ #category : 'visiting' }
EEvaluatorVisitor >> at: aKey put: aValue [
	bindings at: aKey put: aValue
]

{ #category : 'visiting' }
EEvaluatorVisitor >> initialize [
	super initialize.
	bindings := Dictionary new
]

{ #category : 'visiting' }
EEvaluatorVisitor >> visitAddition: anAddition [
	^ (anAddition left accept: self) + (anAddition right accept: self)
]

{ #category : 'visiting' }
EEvaluatorVisitor >> visitConstant: aConstant [
	^ aConstant value
]

{ #category : 'visiting' }
EEvaluatorVisitor >> visitMultiplication: aMultiplication [
	^ (aMultiplication left accept: self) * (aMultiplication right accept: self)
]

{ #category : 'visiting' }
EEvaluatorVisitor >> visitNegation: aNegation [ 
	^ (aNegation expression accept: self) negated
]

{ #category : 'visiting' }
EEvaluatorVisitor >> visitVariable: aVariable [
	^ bindings at: aVariable name
]
