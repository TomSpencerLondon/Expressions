Class {
	#name : 'EAdditionTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EAdditionTest >> testClassCreation [
	self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 8
]

{ #category : 'tests' }
EAdditionTest >> testEvaluate [
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: (EAddition new right: ep1; left: ep2) evaluate equals: 8
]
