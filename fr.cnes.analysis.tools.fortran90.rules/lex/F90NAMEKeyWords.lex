/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.NAME.KeyWords rule.  */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*	                                                                         */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;

import java.util.List;

import org.eclipse.core.runtime.IPath;




import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F90NAMEKeyWords
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT, NAMING, NEW_LINE, LINE, DECLARATION, DECL_NAME

COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
END			 = END 		  | end
MOTS_CLES	 = ALLOCATABLE| allocatable | ALLOCATE | allocate | ASSIGN | assign | 
			   BACKSPACE | backspace | BLOCK[\ ]DATA | block[\ ]data | CALL | call | 
			   CASE | case | CLOSE | close | COMMON | common | CONTAINS | contains | 
			   CONTINUE | continue | CYCLE | cycle | DATA |data | DEALLOCATE | deallocate | 
			   DIMENSION | dimension | DO | do | ELSE[\ ]IF |else[\ ]if | ELSE | else | 
			   ELSEWHERE | elsewhere | END | end | ENDFILE | endfile | ENDIF |endif | 
			   ENTRY | entry | EQUIVALENCE | equivalence | EXIT | exit | EXTERNAL |external | 
			   FORMAT | format | FUNCTION | function | GOTO | goto | IF | if | 
			   IMPLICIT | implicit | INCLUDE | include | INQUIRE | inquire | INTENT | intent | 
			   INTERFACE | interface | INTRINSIC | intrinsic | MODULE | module | 
			   NAMELIST | namelist | NULLIFY | nullify | ONLY | only | OPEN | open | 
			   OPERATOR | operator | OPTIONAL | optional | PARAMETER | parameter | 
			   PAUSE | pause | POINTER | pointer | PRINT | print | PRIVATE | private | 
			   PROCEDURE | procedure | PROGRAM | program | PUBLIC | public | READ | read | 
			   RECURSIVE | recursive | RESULT | result | RETURN | return | REWIND | rewind | 
			   REWRITE | rewrite | SAVE | save | SELECT | select | SEQUENCE | sequence | 
			   STOP | stop | SUBROUTINE | subroutine | TARGET | target | THEN | then | 
			   USE | use | WHERE | where | WHILE | while | WRITE | write
FUNC_INTRIN  = ABS | abs | ACHAR | ACOS | ADJUSTL | ADJUSTR | AIMAG | AINT | ALL | ALLOCATED | 
		       ANINT | ANY | ASIN | ASSOCIATED | ATAN | ATAN2 | BIT_SIZE | BTEST | CEILING | CHAR | 
		       CMPLX | CONJG | COS | COSH | COUNT | CSHIFT | DBLE | DIGITS | DIM | DPROD | EOSHIFT | 
		       EPSILON | EXP | EXPONENT | FLOOR | FRACTION | HUGE | IACHAR | IAND | IBCLR.  IBITS | 
		       IBSET | ICHAR | IEOR | INDEX | INT | INTENT | IOR | ISHFT |  ISHFTC | KIND | LBOUND | 
		       LBOUND | LEN | LEN_TRIM | LGE | LGT | LLE | LLT | LOG | LOG10 | LOGICAL | MAX | 
		       MAXEXPONENT | MAXLOC | MAXVAL | MERGE | MIN | MINEXPONENT | MINLOC | MINVAL | MOD | 
		       MODULO | MVBITS | NEAREST | NINT | NOT | PACK | PRECISION | PRESENT | PRODUCT | RADIX | 
		       RANGE | REAL | REPEAT | RESHAPE | RRSPACING | SCALE | SCAN | SELECTED_INT_KIND | 
		       SELECTED_REAL_KIND | SET_EXPONENT | SHAPE | SIGN | SIN | SINH | SIZE | SPACING | SPREAD | 
		       SQRT | SUM | TAN | TANH | TINY | TRANSFER | TRANSPOSE | TRIM | UBOUND | UNPACK | VERIFY |
		       abs | achar | acos | adjustl | adjustr | aimag | aint | all | allocated | anint | any | 
		       asin | associated | atan | atan2 | bit_size | btest | ceiling | char | cmplx | conjg | 
		       cos | cosh | count | cshift | dble | digits | dim | dprod | eoshift | epsilon | exp | 
		       exponent | floor | fraction | huge | iachar | iand | ibclr.  ibits | ibset | ichar | ieor | 
		       index | int | intent | ior | ishft |  ishftc | kind | lbound | lbound | len | len_trim | 
		       lge | lgt | lle | llt | log | log10 | logical | max | maxexponent | maxloc | maxval | merge | 
		       min | minexponent | minloc | minval | mod | modulo | mvbits | nearest | nint | not | pack | 
		       precision | present | product | radix | range | real | repeat | reshape | rrspacing | scale | 
		       scan | selected_int_kind | selected_real_kind | set_exponent | shape | sign | sin | sinh | 
		       size | spacing | spread | sqrt | sum | tan | tanh | tiny | transfer | transpose | trim | 
		       ubound | unpack | verify
SUB_INTRIN	 = DATE_AND_TIME | MVBITS | RANDOM_NUMBER | RANDOM_SEED | SYSTEM_CLOCK | 
			   date_and_time | mvbits | random_number | random_seed | system_clock
INTRIN       = {SUB_INTRIN} | {FUNC_INTRIN}
CLE	         = {MOTS_CLES} | {INTRIN}
DATA_TYPE    = REAL | real | DOUBLE[\ ]+PRECISION | double[\ ]+precision | INTEGER | integer | LOGICAL | logical |
			   CHARACTER | character | COMPLEX | complex
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f]

%{
	String location = "MAIN PROGRAM"; 
  List<Violation> list = new LinkedList<Violation>();
	boolean end = true;
	
	public F90NAMEKeyWords() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
%}

%eofval{ 
  
 return getViolations(); 
%eofval}

/* Transition word is entry (or ENTRY). This word must not be found. */
/* Whenever it's found, an error is returned.						 */

%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE        */
/************************/
<NAMING>		{INTRIN}		{if (location.contains("FUNCTION") || location.contains("SUBROUTINE")) {
									location = location + " " + yytext();setError(location,"The variable "+yytext().toUpperCase()+" is a keyword in Fortran90 language.", yyline+1);
								 } yybegin(COMMENT);} 
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		"intrinsic"		{yybegin(COMMENT);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECLARATION);}
<NEW_LINE>		{END}			{yybegin(COMMENT);}
<NEW_LINE>		{VAR}			{yybegin(LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			"intrinsic"		{yybegin(COMMENT);}
<LINE>			{DATA_TYPE}		{yybegin(DECLARATION);}
<LINE>			{END}			{yybegin(COMMENT);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>		{STRING}		{}
<DECLARATION>	  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<DECLARATION>		\:\:         	{yybegin(DECL_NAME);}
<DECLARATION>      	\n             	{yybegin(NEW_LINE);}
<DECLARATION>      	.              	{}


/************************/
/* DECL_NAME   STATE    */
/************************/
<DECL_NAME>		{STRING}		{end=true;}
<DECL_NAME>		{CLE}			{end=true; setError(location,"The variable "+yytext()+" is a keyword in Fortran90 language.", yyline+1);}
<DECL_NAME>		{VAR}         	{end=true;}
<DECL_NAME>		{SPACE}			{}
<DECL_NAME>		\&				{end=false;}
<DECL_NAME>     \n             	{if(end)yybegin(NEW_LINE);}
<DECL_NAME>     .              	{end=true;}


/************************/
/* THROW ERROR          */
/************************/
				[^]           {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
