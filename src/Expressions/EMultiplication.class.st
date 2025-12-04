Class {
	#name : 'EMultiplication',
	#superclass : 'Object',
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
