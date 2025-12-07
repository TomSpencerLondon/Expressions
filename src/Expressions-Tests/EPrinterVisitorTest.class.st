Class {
	#name : 'EPrinterVisitorTest',
	#superclass : 'TestCase',
	#category : 'Expressions-Tests',
	#package : 'Expressions-Tests'
}

{ #category : 'tests' }
EPrinterVisitorTest >> testVisitAddition [
	| expr |
	expr := EAddition left: (EConstant value: 3) right: (EConstant value: 5).
	self assert: (expr accept: EPrinterVisitor new) equals: '(3 + 5)'
]

{ #category : 'tests' }
EPrinterVisitorTest >> testVisitContent [
	self assert: ((EConstant value: 5) accept: EPrinterVisitor new) equals: '5'
]

{ #category : 'tests' }
EPrinterVisitorTest >> testVisitMultiplication [
	| expr |
	expr := EMultiplication left: (EConstant value: 3) right: (EConstant value: 5).
	self assert: (expr accept: EPrinterVisitor new) equals: '(3 * 5)'
]

{ #category : 'tests' }
EPrinterVisitorTest >> testVisitNegation [
	self assert: ((EConstant value: 5) negated accept: EPrinterVisitor new) equals: '-(5)'
]

{ #category : 'tests' }
EPrinterVisitorTest >> testVisitVariable [
	self assert: ((EVariable named: 'x') accept: EPrinterVisitor new) equals: 'x'
]
