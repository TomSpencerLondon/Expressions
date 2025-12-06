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
	#superclass : 'EBinaryExpression',
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'instance creation' }
EMultiplication class >> left: anExpression right: anotherExpression [ 
	^ self new left: anExpression; right: anotherExpression
]

{ #category : 'instance creation' }
EMultiplication class >> sampleInstance [
    <sampleInstance>
    ^ self left: (EConstant value: 3) right: (EConstant value: 5)
]

{ #category : 'public access' }
EMultiplication >> evaluate [
	^ left evaluate * right evaluate
]

{ #category : 'accessing' }
EMultiplication >> left: anExpression [
	left := anExpression
]

{ #category : 'public access' }
EMultiplication >> operatorString [
	^ ' * '
]

{ #category : 'accessing' }
EMultiplication >> right: anExpression [
	right := anExpression
]
