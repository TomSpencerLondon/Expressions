Class {
	#name : 'EAddition',
	#superclass : 'Object',
	#instVars : [
		'right',
		'left'
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
EAddition >> right: anExpression [
	right := anExpression
]
