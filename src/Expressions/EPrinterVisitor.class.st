"
I am a visitor that converts expression trees to their string representation.

I implement the Visitor pattern - expressions accept me via accept:, and I define visitXxx: methods for each expression type to build the string output.

Unlike EEvaluatorVisitor, I don't need any state - I simply traverse the tree and concatenate strings.

Example:
    | expr result |
    expr := EAddition left: (EConstant value: 3) right: (EConstant value: 5).
    result := expr accept: EPrinterVisitor new.  ""Returns '(3 + 5)'""

Example with nested expression:
    | expr result |
    expr := EMultiplication 
        left: (EAddition left: (EConstant value: 3) right: (EConstant value: 4))
        right: (EConstant value: 5).
    result := expr accept: EPrinterVisitor new.  ""Returns '((3 + 4) * 5)'""

Example with variable:
    | expr result |
    expr := EAddition left: (EVariable named: 'x') right: (EConstant value: 3).
    result := expr accept: EPrinterVisitor new.  ""Returns '(x + 3)'""
"
Class {
	#name : 'EPrinterVisitor',
	#superclass : 'Object',
	#category : 'Expressions',
	#package : 'Expressions'
}

{ #category : 'visiting' }
EPrinterVisitor >> visitAddition: anAddition [ 
	    ^ '(' , (anAddition left accept: self) , ' + ' , (anAddition right accept: self) , ')'

]

{ #category : 'visiting' }
EPrinterVisitor >> visitConstant: aConstant [
	^ aConstant value asString
]

{ #category : 'visiting' }
EPrinterVisitor >> visitMultiplication: aMultiplication [
	^ '(', (aMultiplication left accept: self), ' * ', (aMultiplication right accept: self), ')'
]

{ #category : 'visiting' }
EPrinterVisitor >> visitNegation: aNegation [ 
	^ '-(', (aNegation expression accept: self), ')'
]

{ #category : 'visiting' }
EPrinterVisitor >> visitVariable: aVariable [
	^ aVariable name
]
