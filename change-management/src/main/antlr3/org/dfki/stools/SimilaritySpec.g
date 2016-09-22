grammar SimilaritySpec;

options {
	output=AST;
	backtrack=true;
}

tokens {
	ALTERNATIVES='alternatives';
	ALT;
    ENTRY='ENTRY';
	ALLBUT;
	OPT='?';
	OPTIONAL='OPTIONAL';
	MANDAT='!';
	MANDATORY='mandatory';
    EQUIVSPECS;
	EQUIVSPEC='equivspec';
	EXTENDS='extends';
	XMLNS='xmlns';
	XMLNSPREFIX='xmlns:';
	ELEMENT='element';
	LIMIT='limit';
	ANNOTATIONS='annotations';
	ABSENT='absent';
	CONSTITUENTS='constituents';
	UNORDERED='unordered';
	ORDERED='ordered';
	TEXTNODE='<TEXT>';
	SPLITNODESTART='<SPLIT(';
	SPLITNODEEND=')>';
	SPLITNODE='SPLIT';
	COMMENTNODE='<COMMENT>';
	IGNORE='ignore';
	PREFIXEDNAME;
	UNPREFIXEDNAME;
	TUPLE;
	EQUIVELEM;
	DEFAULTSPEC=':default';
	}

@header{
package org.dfki.xml.stools;
}

@lexer::header{
package org.dfki.xml.stools;
}


equivspecs
    :    equivspec* -> ^(EQUIVSPECS equivspec*)
    ;

equivspec
	:	 EQUIVSPEC^ NCNAME (namespace | extension)? '{'! (xmlnsdecls)* (element | glblmt | ignore )* defaultspec? '}'!
    ;

extension
	:	 EXTENDS NCNAME (','? NCNAME )* -> ^(EXTENDS NCNAME+);

xmlnsdecls
	:	 XMLNSPREFIX^ NCNAME '='! ATTVALUE;

namespace
	:	 XMLNS^ ATTVALUE;

element
	:	 ELEMENT^ simpleelem '{'! alternative? '}'!;

defaultspec 	:	 DEFAULTSPEC^ '{'! alternative? '}'!;

glblmt
	:	 LIMIT^ '{'! (simpleelem ( (','!)? simpleelem )*)? '}'!;

alternative
	:	 ALTERNATIVES^ '{'! ( '{'! altentry '}'! )+ '}'!
               | altentry -> ^(ALTERNATIVES altentry);

altentry
	:	 annotations constituents? -> ^(ENTRY annotations constituents?)
               | constituents annotations? -> ^(ENTRY constituents annotations?)
;

annotations
	:	 ANNOTATIONS^ '{'! annoallbut ((','!)? annoentry)* '}'!
               | ANNOTATIONS^ '{'! annoentry? '}'!;

annoallbut
	:	 '_' ( '\\' '{' qname (',' qname)* '}' )? -> ^(ALLBUT["allbut"] qname*);

annoentry
	:	qnameoptorrequired ((','!)? annoentry)*
	| 	annoabsent ((','!)? annoentry)*;

annoabsent
	:	 ABSENT '{' qname ( (',')? qname )* '}' -> ^(ABSENT qname*);

constituents
	:	 CONSTITUENTS^ '{'! consentry? '}'!;

consentry
	:	 consordered ((','!)? consentry)*
               | consabsent ((','!)? consentry)*
               | consmandatory ((','!)? consentry)*
               | loclmt ((','!)? consentry)*
               ;

consordered
	:	( UNORDERED^ | ORDERED^ ) conscoll
	;

consabsent
	:	 ABSENT '{' simpleelem ( (',')? simpleelem )* '}' -> ^(ABSENT simpleelem*);

consmandatory
	:	 MANDATORY '{' simpleelem ( (',')? simpleelem )* '}' -> ^(MANDATORY simpleelem*);

conscoll
	:	'{'! (conseqelem | conselemtuple) ( (','!)? (conseqelem | conselemtuple) )* '}'!
	| 	'{'! consallbut '}'!;


conselemtuple
	:	'(' conseqelem (',' conseqelem)* ')' -> ^(TUPLE conseqelem+)
	;

conseqelem
	:	simpleelem
	| 	simpleelem equivspecdecl -> ^(EQUIVELEM simpleelem equivspecdecl)
	;

equivspecdecl
	:	'['! NCNAME ']'!
	;

simpleelem
	: 	qname
	|   TEXTNODE
	|   SPLITNODESTART ATTVALUE SPLITNODEEND -> ^(SPLITNODE ATTVALUE)
	| 	COMMENTNODE
	;

consallbut
	:	 '_' ( '\\' '{' (simpleelem ( ',' simpleelem )*)? '}' )?
		-> ^(ALLBUT["allbut"] simpleelem*)
	;


loclmt
	:	 LIMIT '{' (simpleelem ( (',')? simpleelem )*)? '}' -> ^(LIMIT simpleelem*);

ignore
	:	 IGNORE '{' (simpleelem ( (',')? simpleelem )*)? '}' -> ^(IGNORE simpleelem*);

qnameoptorrequired
	:	qname
	| 	qname OPT -> ^(OPTIONAL qname)
	|	qname MANDAT -> ^(MANDATORY qname)
	;

qname	: prefixedName
	| unprefixedName
 	;

prefixedName	   :   NCNAME ':' NCNAME -> ^(PREFIXEDNAME NCNAME+)
	;

unprefixedName	   :  NCNAME -> ^(UNPREFIXEDNAME NCNAME)
	;

ATTVALUE	   :  '"' ( STRINGCONTENT | '\'' )* '"'
	|	      '\'' (STRINGCONTENT | '"' )* '\''
	;

fragment
STRINGCONTENT	: 'a'..'z' | 'A'..'Z' | '\\' | '/' | '=' | '+' | '-' | '.' | '?' | ':' | '0'..'9'
	;

NCNAME 	:	NameStartChar (NameChar)* ;

fragment
NameStartChar	  : 'A'..'Z' | '_' | 'a'..'z';

fragment
NameChar	   :   	NameStartChar | '-' | '.' | '0'..'9';

WS      :       (' ' | '\t' | '\n' | '\r')+
                { $channel = HIDDEN; }
        ;
