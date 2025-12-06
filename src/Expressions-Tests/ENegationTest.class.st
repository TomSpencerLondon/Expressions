Class {
	#name : 'ENegationTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
ENegationTest >> testClassCreation [
	self assert: (EAddition left: (EConstant value: 3) right: (EConstant value: 5)) evaluate equals: 8
]

{ #category : 'tests' }
ENegationTest >> testEvaluate [
	self assert: (ENegation new expression: (EConstant new value: 5)) evaluate equals: -5
]

{ #category : 'tests' }
ENegationTest >> testPrintOn [
	self assert: (ENegation expression: (EConstant value: 5)) printString equals: '-(5)'
]
