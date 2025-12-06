"
I represent the addition of two expressions.

I evaluate both sub-expressions and return their sum.

Example:
    | left right |
    left := EConstant new value: 3.
    right := EConstant new value: 5.
    (EAddition new left: left; right: right) evaluate  ""Returns 8""

Instance Variables:
    left - the left operand expression
    right - the right operand expression
"
Class {
	#name : 'EAddition',
	#superclass : 'EExpression',
	#instVars : [
		'left',
		'right'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'accessing' }
EAddition >> evaluate [
	^ left evaluate + right evaluate
]

{ #category : 'accessing' }
EAddition >> left: anExpression [
	left := anExpression
]

{ #category : 'accessing' }
EAddition >> printOn: aStream [
	aStream nextPut: $(.
	left printOn: aStream.
	aStream nextPutAll: ' + '.
	right printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
EAddition >> right: anExpression [
	right := anExpression
]
