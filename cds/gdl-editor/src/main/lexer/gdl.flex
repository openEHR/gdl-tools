/*
 * Copyright 2008 Ayman Al-Sairafi ayman.alsairafi@gmail.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License
 *       at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package se.cambio.cds.gdl.editor.util;


import jsyntaxpane.Token;
import jsyntaxpane.TokenType;
 
%% 

%public
%class GDLLexer
%extends DefaultJFlexLexer
%final
%unicode
%char
%type Token


%{
    /**
     * Create an empty lexer, yyrset will be called later to reset and assign
     * the reader
     */
    public GDLLexer() {
        super();
    }

    @Override
    public int yychar() {
        return yychar;
    }

    private static final byte PARAN     = 1;
    private static final byte BRACKET   = 2;
    private static final byte GT_BRACKET     = 3;

%}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]+

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} 

TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?

/* identifiers */
Identifier = [:jletter:][:jletterdigit:]*

/* integer literals */
DecIntegerLiteral = 0 | [1-9][0-9]*
DecLongLiteral    = {DecIntegerLiteral} [lL]

HexIntegerLiteral = 0 [xX] 0* {HexDigit} {1,8}
HexLongLiteral    = 0 [xX] 0* {HexDigit} {1,16} [lL]
HexDigit          = [0-9a-fA-F]

OctIntegerLiteral = 0+ [1-3]? {OctDigit} {1,15}
OctLongLiteral    = 0+ 1? {OctDigit} {1,21} [lL]
OctDigit          = [0-7]
    
/* floating point literals */        
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

%state STRING, CHARLITERAL, JDOC, JDOC_TAG

%%

<YYINITIAL> {

  /* keywords */
  
  /* definition */
  "definition"                      |
  "gdl_version"                      |
  "id"                        |
  "concept"                         |
  "language"                         |
  "original_language"                        |
  "description"                         |
  "details"                        |
  "lifecycle_state"                        |
  "copyright"                        |
  "keywords"                        |
  "misuse"                        |
  "purpose"                        |
  "use"                        |
  "original_author"                        |
  "other_contributors"                        |
  "other_details"                        |

  /*archetype bindings */
  "archetype_bindings"                     |
  "archetype_id"                           |
  "domain"                       |
  "elements"                         |
  "path"                         |
  "function"                      |
  "predicates"                    |
  "template_id"                   |
  
  /* rules */
  "pre_conditions"                |
  "rules"                        |
  "when"                      |
  "then"                        |
  "priority"                          |
  
  
  /* ontology */
  "ontology"                      |
  "text"                            |
  "term_bindings"                   |
  "bindings"                        |
  "codes"                           |
  "uri"                             |
  "term_definitions"                   |
  "terms"                       |
  "null"                         { return token(TokenType.KEYWORD); }

  /* Type keywords */
  "LANGUAGE"                            |
  "RESOURCE_DESCRIPTION"                |
  "RESOURCE_DESCRIPTION_ITEM"           |
  "GUIDE_DEFINITION"                    |
  "ARCHETYPE_BINDING"                   |
  "ELEMENT_BINDING"                     |
  "RULE"								|
  "GUIDE_ONTOLOGY"                      |
  "TERM_DEFINITION"                     |
  "TERM"                     |
  "TERM_BINDING"                     |
  "BINDING"                     |
  "GUIDE"                       { return token(TokenType.TYPE); }

  "WARNING"                      { return token(TokenType.WARNING); }
  "ERROR"                        { return token(TokenType.ERROR); }

  /* Frequently used types 
  "ArithmeticException"              |
  "UnsupportedOperationException"    { return token(TokenType.TYPE2); }
  */
  
  /* operators */

  "("                            { return token(TokenType.OPERATOR,  PARAN); }
  ")"                            { return token(TokenType.OPERATOR, -PARAN); }
  "<"                            { return token(TokenType.OPERATOR,  GT_BRACKET); }
  ">"                            { return token(TokenType.OPERATOR, -GT_BRACKET); }
  "["                            { return token(TokenType.OPERATOR,  BRACKET); }
  "]"                            { return token(TokenType.OPERATOR, -BRACKET); }
  ";"                            | 
  ","                            | 
  "."                            | 
  "="                            | 
  ">"                            | 
  "<"                            |
  "!"                            | 
  "~"                            | 
  "?"                            | 
  ":"                            | 
  "=="                           | 
  "<="                           | 
  ">="                           | 
  "!="                           | 
  "&&"                           | 
  "||"                           | 
  "++"                           | 
  "--"                           | 
  "+"                            | 
  "-"                            | 
  "*"                            | 
  "/"                            | 
  "&"                            | 
  "|"                            | 
  "^"                            | 
  "%"                            | 
  "<<"                           | 
  ">>"                           | 
  ">>>"                          | 
  "+="                           | 
  "-="                           | 
  "*="                           | 
  "/="                           | 
  "&="                           | 
  "|="                           | 
  "^="                           | 
  "%="                           | 
  "<<="                          | 
  ">>="                          | 
  ">>>="                         { return token(TokenType.OPERATOR); } 
  
  /* string literal */
  \"                             {  
                                    yybegin(STRING); 
                                    tokenStart = yychar; 
                                    tokenLength = 1; 
                                 }

  /* character literal */
  \'                             {  
                                    yybegin(CHARLITERAL); 
                                    tokenStart = yychar; 
                                    tokenLength = 1; 
                                 }

  /* numeric literals */

  {DecIntegerLiteral}            |
  {DecLongLiteral}               |
  
  {HexIntegerLiteral}            |
  {HexLongLiteral}               |
 
  {OctIntegerLiteral}            |
  {OctLongLiteral}               |
  
  {FloatLiteral}                 |
  {DoubleLiteral}                |
  {DoubleLiteral}[dD]            { return token(TokenType.NUMBER); }
  
  // JavaDoc comments need a state so that we can highlight the @ controls
  "/**"                          {  
                                    yybegin(JDOC); 
                                    tokenStart = yychar; 
                                    tokenLength = 3; 
                                 }

  /* comments */
  {Comment}                      { return token(TokenType.COMMENT); }

  /* whitespace */
  {WhiteSpace}                   { }

  /* identifiers */ 
  {Identifier}                   { return token(TokenType.IDENTIFIER); }
}


<STRING> {
  \"                             { 
                                     yybegin(YYINITIAL); 
                                     // length also includes the trailing quote
                                     return token(TokenType.STRING, tokenStart, tokenLength + 1);
                                 }
  
  {StringCharacter}+             { tokenLength += yylength(); }

  \\[0-3]?{OctDigit}?{OctDigit}  { tokenLength += yylength(); }
  
  /* escape sequences */

  \\.                            { tokenLength += 2; }
  {LineTerminator}               { yybegin(YYINITIAL);  }
}

<CHARLITERAL> {
  \'                             { 
                                     yybegin(YYINITIAL); 
                                     // length also includes the trailing quote
                                     return token(TokenType.STRING, tokenStart, tokenLength + 1);
                                 }
  
  {SingleCharacter}+             { tokenLength += yylength(); }
  
  /* escape sequences */

  \\.                            { tokenLength += 2; }
  {LineTerminator}               { yybegin(YYINITIAL);  }
}

<JDOC> {
  "*/"                           { 
                                     yybegin(YYINITIAL); 
                                     return token(TokenType.COMMENT, tokenStart, tokenLength + 2);
                                 }

  "@"                            {   
                                     yybegin(JDOC_TAG); 
                                     int start = tokenStart;
                                     tokenStart = yychar;
                                     int len = tokenLength;
                                     tokenLength = 1;
                                     return token(TokenType.COMMENT, start, len);
                                 }

  .|\n                           { tokenLength ++; }

}

<JDOC_TAG> {
  ([:letter:])+ ":"?             { tokenLength += yylength(); }

  "*/"                           { 
                                     yybegin(YYINITIAL); 
                                     return token(TokenType.COMMENT, tokenStart, tokenLength + 2);
                                 }

  .|\n                           {   
                                     yybegin(JDOC); 
                                     // length also includes the trailing quote
                                     int start = tokenStart;
                                     tokenStart = yychar;
                                     int len = tokenLength;
                                     tokenLength = 1;
                                     return token(TokenType.COMMENT2, start, len);
                                 }
}


/* error fallback */
.|\n                             {  }
<<EOF>>                          { return null; }

