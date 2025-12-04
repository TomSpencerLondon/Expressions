Class {
	#name : 'ENegation',
	#superclass : 'Object',
	#instVars : [
		'expression'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'instance creation' }
ENegation >> evaluate [
	^ expression evaluate negated 
]

{ #category : 'instance creation' }
ENegation >> expression: anExpression [ 
	expression := anExpression
]
