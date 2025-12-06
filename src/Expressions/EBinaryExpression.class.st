"
I am the abstract superclass for binary expressions (expressions with two operands).

I factor out the common structure of left and right operands shared by EAddition, EMultiplication, and other binary operations.

Subclasses must implement:
- evaluate - to define how the two operands are combined

Instance Variables:
    left - the left operand expression
    right - the right operand expression

Example (using a concrete subclass):
    EAddition left: (EConstant value: 3) right: (EConstant value: 5)'.

"
Class {
	#name : 'EBinaryExpression',
	#superclass : 'EExpression',
	#instVars : [
		'left',
		'right'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'instance creation' }
EBinaryExpression class >> left: anExpression right: anotherExpression [ 
	^ self new left: anExpression; right: anotherExpression
]

{ #category : 'instance creation' }
EBinaryExpression >> left: anExpression [
	left := anExpression
]

{ #category : 'instance creation' }
EBinaryExpression >> left: anExpression right: anotherExpression [
	^ self new left: anExpression; right: anotherExpression
]

{ #category : 'instance creation' }
EBinaryExpression >> printOn: aStream [
	aStream nextPut: $(.
	left printOn: aStream.
	aStream nextPutAll: self operatorString.
	right printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'instance creation' }
EBinaryExpression >> right: anExpression [
	right := anExpression
]
