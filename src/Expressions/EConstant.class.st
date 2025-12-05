"
I represent a constant numeric value in an expression tree.

I am a leaf node - I have no sub-expressions.

Example:
    (EConstant new value: 5) evaluate  ""Returns 5""

Instance Variables:
    value - the numeric value I hold
"
Class {
	#name : 'EConstant',
	#superclass : 'EExpression',
	#instVars : [
		'value'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'accessing' }
EConstant >> evaluate [
	^ value
]

{ #category : 'arithmetic' }
EConstant >> negated [
	^ ENegation new expression: self
]

{ #category : 'accessing' }
EConstant >> value: anInteger [ 
	value := anInteger
]
