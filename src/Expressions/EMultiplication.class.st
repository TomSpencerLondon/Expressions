"
I represent the multiplication of two expressions.

I evaluate both sub-expressions and return their product.

Example:
    | left right |
    left := EConstant new value: 3.
    right := EConstant new value: 5.
    (EMultiplication new left: left; right: right) evaluate  ""Returns 15""

Instance Variables:
    left - the left operand expression
    right - the right operand expression
"
Class {
	#name : 'EMultiplication',
	#superclass : 'EExpression',
	#instVars : [
		'right',
		'left'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'public access' }
EMultiplication >> evaluate [
	^ left evaluate * right evaluate
]

{ #category : 'accessing' }
EMultiplication >> left: anExpression [
	left := anExpression
]

{ #category : 'accessing' }
EMultiplication >> right: anExpression [
	right := anExpression
]
