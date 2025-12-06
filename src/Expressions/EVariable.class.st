"
I represent a variable in an expression tree.

When evaluated with a dictionary of bindings, I look up my name and return the corresponding value.

Example:
    (EVariable named: 'x') evaluateWith: { 'x' -> 5 } asDictionary  ""Returns 5""

Instance Variables:
    name - the variable name as a String
"
Class {
	#name : 'EVariable',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'instance creation' }
EVariable class >> named: aString [
	^ self new name: aString
]

{ #category : 'accessing' }
EVariable >> evaluateWith: aDictionary [
	^ aDictionary at: name
]

{ #category : 'accessing' }
EVariable >> name: aString [
	name := aString
]

{ #category : 'accessing' }
EVariable >> printOn: aStream [
	aStream nextPutAll: name
]
