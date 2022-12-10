/*
 [The "BSD licence"]
 Copyright (c) 2013 Sam Harwell
 All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

parser grammar C11ParserGrammar;

options {
    tokenVocab = C11LexerGrammar;
}

/** C 2011 grammar built from the C11 Spec */

primaryExpression
    :   Identifier          #primaryExpression_Identifier
    |   Constant            #primaryExpression_Constant
    |   StringLiteral+      #primaryExpression_StringLiteral
    |   '(' expression ')'  #primaryExpression_Parenthesis
    |   genericSelection    #primaryExpression_GenericSelection
    ;

genericSelection
    :   '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

genericAssocList
    :   genericAssociation (',' genericAssociation)*
    ;

genericAssociation
    :   (typeName | 'default') ':' assignmentExpression
    ;

postfixExpression
    :  primaryExpression                                  #postfixExpression_NoPostfix
    | postfixExpression '[' expression ']'                #postfixExpression_ArraySubscripting
    | postfixExpression '(' argumentExpressionList? ')'   #postfixExpression_FunctionCalls  
    | postfixExpression ('.' | '->') Identifier           #postfixExpression_StructureUnionMembers
    | postfixExpression ('++' | '--')                     #postfixExpression_PostfixIncrementDecrement
    | '(' typeName ')' '{' initializerList ','? '}'       #postfixExpression_CompoundLiterals
    ;

argumentExpressionList
    :   assignmentExpression (',' assignmentExpression)*
    ;

unaryExpression
    : postfixExpression                             #unaryExpression_noUnaryExpression
    | ('++' |  '--' | 'sizeof' )+ unaryExpression   #unaryExpression_nestedUnaryExpression
    | unaryOperator castExpression                  #unaryExpression_castExpression
    | ( 'sizeof' | '_Alignof' ) '(' typeName ')'    #unaryExpression_unaryExpressionOnType
    ;

unaryOperator
    :   '&' | '*' | '+' | '-' | '~' | '!'  // unary operators cannot nest
    ;

castExpression
    :   '(' typeName ')' castExpression     #castExpression_casting
    |   unaryExpression                     #castExpression_nocasting
    ;

multiplicativeExpression
    :   castExpression                                  #multiplicativeExpression_noMultiplicative
    |   castExpression op=('*'|'/'|'%') castExpression  #multiplicativeExpression_MulORDivORMod
    ;

additiveExpression
    :   multiplicativeExpression                                        #additiveExpression_NoAdditive
    |   multiplicativeExpression ('+'|'-') multiplicativeExpression  #additiveExpression_AddORSub
    ;

shiftExpression    
    :   additiveExpression                                      #shiftExpression_NoShiftExpression
     |  additiveExpression ('<<'|'>>') additiveExpression    #shiftExpression_ShiftLeftORRight
    ;

relationalExpression
    :   shiftExpression                                     #relationalExpression_NorelationalExpression 
    |   shiftExpression ('<'|'>'|'<='|'>=') shiftExpression #relationalExpression_LessORGreaterORLessEqORGreaterEq
    ;

equalityExpression
    :   relationalExpression                                    #equalityExpression_NoEqualityExpression
     |  relationalExpression ('=='| '!=') relationalExpression  #equalityExpression_EqualORNotEqual
    ;

andExpression
    :   equalityExpression                          #andExpression_NoAndExpression                         
    |   equalityExpression '&' equalityExpression   #andExpression_AndExpression
    ;

exclusiveOrExpression
    :   andExpression                   #exclusiveOrExpression_NoExclusiveOrExpression
    |   andExpression '^' andExpression #exclusiveOrExpression_ExclusiveOrExpression
    ;

inclusiveOrExpression
    :   exclusiveOrExpression                            #inclusiveOrExpression_NoInclusiveOrExpression                            
    | exclusiveOrExpression  '^'  exclusiveOrExpression  #inclusiveOrExpression_InclusiveOrExpression
    ;

logicalAndExpression
    :   inclusiveOrExpression                               #logicalAndExpression_NoLogicalAndExpression 
    |   inclusiveOrExpression  '&&' inclusiveOrExpression   #logicalAndExpression_LogicalANDExpression
    ;

logicalOrExpression
    :   logicalAndExpression                                #logicalOrExpression_NologicalOrExpression
     |  logicalAndExpression  '||' logicalAndExpression     #logicalOrExpression_LogicalORExpression
    ;

conditionalExpression
    :   logicalOrExpression                                             #conditionalExpression_NoconditionalExpression 
    |   logicalOrExpression '?' expression ':' conditionalExpression    #conditionalExpression_ConditionalExpression
    ;

assignmentExpression
    :   conditionalExpression                                       #assignmentExpression_NoassignmentExpression
    |   unaryExpression assignmentOperator assignmentExpression     #assignmentExpression_AssignmentExpression
    ;

assignmentOperator
    :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

expression
    :   assignmentExpression                                #expression_SingleExpression
     |  assignmentExpression (',' assignmentExpression)*    #expression_CommaExpression
    ;

constantExpression
    :   conditionalExpression
    ;

declaration
    :   declarationSpecifiers initDeclaratorList? ';'   #declaration_declaration
    |   staticAssertDeclaration                         #declaration_StaticAssertDeclaration
    ;

declarationSpecifiers
    :   declarationSpecifier+
    ;

declarationSpecifiers2
    :   declarationSpecifier+
    ;

declarationSpecifier
    :   storageClassSpecifier
    |   typeSpecifier
    |   typeQualifier
    |   functionSpecifier
    |   alignmentSpecifier
    ;

initDeclaratorList
    :   initDeclarator (',' initDeclarator)*
    ;

initDeclarator
    :   declarator ('=' initializer)?
    ;

storageClassSpecifier
    :   'typedef'
    |   'extern'
    |   'static'
    |   '_Thread_local'
    |   'auto'
    |   'register'
    ;

typeSpecifier
    :   ('void'
    |   'char'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'signed'
    |   'unsigned'
    |   '_Bool'
    |   '_Complex')
    |   atomicTypeSpecifier
    |   structOrUnionSpecifier
    |   enumSpecifier
    |   typedefName    
    ;

structOrUnionSpecifier
    :   structOrUnion Identifier? '{' structDeclarationList '}'
    |   structOrUnion Identifier
    ;

structOrUnion
    :   'struct'
    |   'union'
    ;

structDeclarationList
    :   structDeclaration+
    ;

structDeclaration // The first two rules have priority order and cannot be simplified to one expression.
    :   specifierQualifierList structDeclaratorList ';'
    |   specifierQualifierList ';'
    |   staticAssertDeclaration
    ;

specifierQualifierList
    :   (typeSpecifier| typeQualifier) specifierQualifierList?
    ;

structDeclaratorList
    :   structDeclarator (',' structDeclarator)*
    ;

structDeclarator
    :   declarator
    |   declarator? ':' constantExpression
    ;

enumSpecifier
    :   'enum' Identifier? '{' enumeratorList ','? '}'
    |   'enum' Identifier
    ;

enumeratorList
    :   enumerator (',' enumerator)*
    ;

enumerator
    :   enumerationConstant ('=' constantExpression)?
    ;

enumerationConstant
    :   Identifier
    ;

atomicTypeSpecifier
    :   '_Atomic' '(' typeName ')'
    ;

typeQualifier
    :   'const'
    |   'restrict'
    |   'volatile'
    |   '_Atomic'
    ;

functionSpecifier
    :   ('inline'
    |   '_Noreturn')   
    ;

alignmentSpecifier
    :   '_Alignas' '(' (typeName | constantExpression) ')'
    ;

declarator
    :   pointer? directDeclarator 
    ;

directDeclarator
    :   Identifier
    |   '(' declarator ')'
    |   directDeclarator '[' typeQualifierList? assignmentExpression? ']'
    |   directDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    |   directDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    |   directDeclarator '[' typeQualifierList? '*' ']'
    |   directDeclarator '(' parameterTypeList ')'
    |   directDeclarator '(' identifierList? ')'
    |   Identifier ':' DigitSequence  // bit field
    
    ;

nestedParenthesesBlock
    :   (   ~('(' | ')')
        |   '(' nestedParenthesesBlock ')'
        )*
    ;

pointer
    :  (('*'|'^') typeQualifierList?)+ // ^ - Blocks language extension
    ;

typeQualifierList
    :   typeQualifier+
    ;

parameterTypeList
    :   parameterList (',' '...')?
    ;

parameterList
    :   parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    :   declarationSpecifiers declarator
    |   declarationSpecifiers2 abstractDeclarator?
    ;

identifierList
    :   Identifier (',' Identifier)*
    ;

typeName
    :   specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    :   pointer
    |   pointer? directAbstractDeclarator 
    ;

directAbstractDeclarator
    :   '(' abstractDeclarator ')' 
    |   '[' typeQualifierList? assignmentExpression? ']'
    |   '[' 'static' typeQualifierList? assignmentExpression ']'
    |   '[' typeQualifierList 'static' assignmentExpression ']'
    |   '[' '*' ']'
    |   '(' parameterTypeList? ')' 
    |   directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']'
    |   directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    |   directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    |   directAbstractDeclarator '[' '*' ']'
    |   directAbstractDeclarator '(' parameterTypeList? ')' 
    ;

typedefName
    :   Identifier
    ;

initializer
    :   assignmentExpression
    |   '{' initializerList ','? '}'
    ;

initializerList
    :   designation? initializer (',' designation? initializer)*
    ;

designation
    :   designatorList '='
    ;

designatorList
    :   designator+
    ;

designator
    :   '[' constantExpression ']'
    |   '.' Identifier
    ;

staticAssertDeclaration
    :   '_Static_assert' '(' constantExpression ',' StringLiteral+ ')' ';'
    ;

statement
    :   labeledStatement
    |   compoundStatement
    |   expressionStatement
    |   selectionStatement
    |   iterationStatement
    |   jumpStatement   
    ;

labeledStatement
    :   Identifier ':' statement
    |   'case' constantExpression ':' statement
    |   'default' ':' statement
    ;

compoundStatement
    :   '{' blockItemList? '}'
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   statement
    |   declaration
    ;

expressionStatement
    :   expression? ';'
    ;

selectionStatement
    :   'if' '(' expression ')' statement ('else' statement)?
    |   'switch' '(' expression ')' statement
    ;

iterationStatement
    :   While '(' expression ')' statement
    |   Do statement While '(' expression ')' ';'
    |   For '(' forCondition ')' statement
    ;

//    |   'for' '(' expression? ';' expression?  ';' forUpdate? ')' statement
//    |   For '(' declaration  expression? ';' expression? ')' statement

forCondition
	:   (forDeclaration | expression?) ';' forExpression? ';' forExpression?
	;

forDeclaration
    :   declarationSpecifiers initDeclaratorList?
    ;

forExpression
    :   assignmentExpression (',' assignmentExpression)*
    ;

jumpStatement
    :   ('goto' Identifier
    |   ('continue'| 'break')
    |   'return' expression?
    |   'goto' unaryExpression // GCC extension
    )
    ';'
    ;

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   externalDeclaration+
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    |   ';' // stray ;
    ;

functionDefinition
    :   declarationSpecifiers? declarator declarationList? compoundStatement
    ;

declarationList
    :   declaration+
    ;
