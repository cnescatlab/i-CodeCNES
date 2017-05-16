/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DATA.Declaration rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DATADeclaration	
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, WAIT, IMPLICITE, INTERFACE, WAIT_2, SAVE, WAIT_3

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

COMMENT_WORD = \!
COMMENT_LINE = \![^\n]*\n
TYPE		 = "function" | "procedure"| "subroutine"	| "program" | "module" 
END_TYPE 	 = "end"{SPACE}*{TYPE}
DATA_TYPE 	 = ("integer"  | "real"	   | "complex" 		| "double"{SPACE}*"precision"	|
			   "logical"  | "character"	| "type") ({SPACE}*\()?	
MOTS_CLES	 = ALLOCATABLE| allocatable | ALLOCATE | allocate | ASSIGN | assign | 
			   BACKSPACE | backspace | BLOCK[\ ]DATA | block[\ ]data | CALL | call | 
			   CASE | case | CLOSE | close | COMMON | common | CONTAINS | contains | 
			   CONTINUE | continue | CYCLE | cycle | DATA |data | DEALLOCATE | deallocate | 
			   DIMENSION | dimension | DO | do | ELSE[\ ]IF |else[\ ]if | ELSE | else | 
			   ELSEWHERE | elsewhere | END | end | ENDDO |enddo |ENDFILE | endfile | ENDIF |endif | 
			   ENTRY | entry | ENDTYPE |endtype | EQ | eq | EQUIVALENCE | equivalence | EXIT | exit | EXTERNAL |external | 
			   FALSE | false | FORMAT | format | FUNCTION | function | GE | ge | GOTO | goto | GT | gt |IF | if | 
			   IMPLICIT | implicit | INCLUDE | include | INQUIRE | inquire | INTENT | intent | 
			   INTERFACE | interface | INTRINSIC | intrinsic | LE | le | LT | lt | MODULE | module | 
			   NAMELIST | namelist | NE | ne | NONE | none | NULLIFY | nullify | ONLY | only | OPEN | open | 
			   OPERATOR | operator | OPTIONAL | optional | PARAMETER | parameter | 
			   PAUSE | pause | POINTER | pointer | PRINT | print | PRIVATE | private | 
			   PROCEDURE | procedure | PROGRAM | program | PUBLIC | public | READ | read | 
			   RECURSIVE | recursive | RESULT | result | RETURN | return | REWIND | rewind | 
			   REWRITE | rewrite | SAVE | save | SELECT | select | SEQUENCE | sequence | 
			   STOP | stop | SUBROUTINE | subroutine | TARGET | target | THEN | then | TRUE | true | 
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

VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f\r]

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
%{
	String location = "MAIN PROGRAM"; 
	/** LIst of violations **/
  	List<Violation> list = new LinkedList<Violation>();
  	int line = 0;
  	/** List of variables**/
  	List<String> variables = new LinkedList<String>();
  	/** List of possible error **/
  	List<String> possibleError = new LinkedList<String>();
  	/** List of location error **/
  	List<String> errorLocation = new LinkedList<String>();
	/** List of possible error line **/	
	List<Integer> errorLine = new LinkedList<Integer>();
	int parenthese = 0;
	
	public F90DATADeclaration() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void comparerError()throws JFlexException{
		for(int i=0; i < possibleError.size(); i++){
			if(!variables.contains(possibleError.get(i))){
				setError(errorLocation.get(i),"The variable "+ possibleError.get(i) +" must be declared." , errorLine.get(i)+1); 
				variables.add(possibleError.get(i));
			}
		}
	}
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
	comparerError();
 	return getViolations(); 
%eofval}


%%          


				

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT> 
		{
		  	\n             	{yybegin(NEW_LINE);}  
   			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>
		{
			{VAR}			{location = location + " " + yytext(); variables.add(yytext().toLowerCase()); line = yyline+1; yybegin(WAIT);}
		    \n             	{line = yyline + 1; yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* WAIT			        */
/************************/
<WAIT>
		{
			\&{SPACE}*\n	{}
			\n				{yybegin(IMPLICITE);}
			.				{}
		} 
		
		
/************************/
/* IMPLICITE	        */
/************************/
<IMPLICITE>
		{
			{COMMENT_LINE}				{}
			"use"						{yybegin(WAIT);}
			"implicit"{SPACE}*"none"	{yybegin(NEW_LINE);}
			{DATA_TYPE}					{setError(location,"The sequence IMPLICIT NONE must be declared after the method. ", line); yybegin(LINE);
										 yybegin(WAIT_2);}
			{VAR}						{setError(location,"The sequence IMPLICIT NONE must be declared after the method. ", line); yybegin(LINE);}
			[^]						{}
		} 		

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL> 
		{
		 	{COMMENT_LINE} 	{}
			{STRING}		{yybegin(LINE);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			{END_TYPE}		{}
 			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>
		{
		  	{COMMENT_WORD} 			{yybegin(COMMENT);}
			{STRING}				{yybegin(LINE);}
			"COMMON" | "NAMELIST"	{yybegin(SAVE);}
			{TYPE}        			{location = yytext(); yybegin(NAMING);}
			"interface"				{yybegin(INTERFACE);}
			{END_TYPE}				{yybegin(COMMENT);}
			{DATA_TYPE}				{yybegin(WAIT_2);}
			"use"					{yybegin(COMMENT);}
			"private"				{yybegin(SAVE);}
			{CLE}					{}
			(\%{VAR})+				{}
			{VAR}{SPACE}*\(			{parenthese=1; yybegin(WAIT_3);}
			{VAR}					{if (!variables.contains(yytext())){
										possibleError.add(yytext().toLowerCase());
										errorLocation.add(location);
										errorLine.add(yyline);}}
			{SPACE}					{}
		 	\n             			{}
		  	.              			{yybegin(LINE);}
		}



/************************/
/* LINE STATE           */
/************************/
<LINE>
		{
			
		  	{COMMENT_WORD} 			{yybegin(COMMENT);}
			{STRING}				{yybegin(LINE);}
			{TYPE}        			{location = yytext(); yybegin(NAMING);}
			"COMMON" | "NAMELIST"	{yybegin(SAVE);}
			"interface"				{yybegin(INTERFACE);}
			{END_TYPE}				{yybegin(COMMENT);}
			"use"					{yybegin(COMMENT);}
			"private"				{yybegin(SAVE);}
			{CLE}					{}
			(\%{VAR})+				{}
			{VAR}{SPACE}*\(			{parenthese=1; yybegin(WAIT_3);}
			{VAR}					{if (!variables.contains(yytext())){
										possibleError.add(yytext().toLowerCase());
										errorLocation.add(location);
										errorLine.add(yyline);}}					
			{SPACE}					{}
      		\n             			{yybegin(NEW_LINE);}
      		.              			{}
      	}
      	
/************************/
/* WAIT_3       		 */
/************************/
<WAIT_3>
		{
			\(				{parenthese++;}
			\)				{parenthese--;
							 if (parenthese==0) 
							 yybegin(LINE);}
      		[^]         	{}	
		}   	
      	
 /************************/
/* WAIT_2       		 */
/************************/
<WAIT_2>
		{
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			\:\:			{yybegin(SAVE);}
      		[^]         	{}	
		}
		
 /************************/
/* SAVE		       		 */
/************************/
<SAVE>
		{
			{VAR}			{variables.add(yytext().toLowerCase());}
			\&{SPACE}*\n	{}
			\n             	{yybegin(NEW_LINE);}
      		.              	{}
		}		
		
      	
/************************/
/* INTERFACE       */
/************************/
<INTERFACE>
		{
			"end"{SPACE}*"interface"	{yybegin(LINE);}
			[^]        				{}
		}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}

