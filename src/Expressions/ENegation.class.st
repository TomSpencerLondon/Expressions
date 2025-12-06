"
I represent the negation of an expression.

I wrap another expression and return its negated value when evaluated.

Example:
    (ENegation new expression: (EConstant new value: 5)) evaluate  ""Returns -5""
    (EConstant new value: 5) negated evaluate  ""Same thing, nicer syntax""

Instance Variables:
    expression - the expression to negate
"
Class {
	#name : 'ENegation',
	#superclass : 'EExpression',
	#instVars : [
		'expression'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'as yet unclassified' }
ENegation class >> sampleInstance [
    <sampleInstance>
    ^ self expression: (EConstant value: 5)
]

{ #category : 'instance creation' }
ENegation >> evaluate [
	^ expression evaluate negated 
]

{ #category : 'instance creation' }
ENegation >> expression: anExpression [ 
	expression := anExpression
]

{ #category : 'instance creation' }
ENegation >> negated [
	^ expression
	
]

{ #category : 'instance creation' }
ENegation >> printOn: aStream [
	aStream nextPut: $-.
	aStream nextPut: $(.
	expression printOn: aStream.
	aStream nextPut: $)
]
