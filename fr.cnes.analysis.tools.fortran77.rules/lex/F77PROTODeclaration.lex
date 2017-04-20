/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for F77.PROTO.Declaration rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.		*/
/* As many comments have been done on the ExampleRule.lex file, this file	    */
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77PROTODeclaration
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/*
 * STATES
 */
%state COMMENT, NAMING, NEW_LINE, LINE, FUNCTION, EXT, RIGHT_SIDE,DECLARATION, EXTERNAL

COMMENT_WORD = \!         | c          | C		| \*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER | integer | REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
			   double[\ ]+precision | CHARACTER | character | LOGICAL  | logical  | DIMENSION  | dimension
EXTERNAL	 = EXTERNAL	  | external	|
			   INTRINSIC  | intrinsic
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| \*
																
%{
	String location = "MAIN PROGRAM";
	/** List of external functions declared on file as EXTERNAL**/
	List<String> externalFunctions 		= new LinkedList<String>();
	/**List of internal functions declared on file as FUNCTION **/
	List<String> internalFunctions 		= new LinkedList<String>();
	/** List of intrinsic functions declared on handbook **/
	List<String> intrinsequesGeneriques = new LinkedList<String>();
	/** List of variables on the file **/
	List<String> variables				= new LinkedList<String>();
	/** List of called functions: CALL or value = FUNC_NAME**/
	List<String> callsFunctions    		= new LinkedList<String>();
	/** Posible list of error locatin **/
	List<String> errorLocation     		= new LinkedList<String>();
	/** Posible list of error lines **/
	List<Integer> errorLine		   		= new LinkedList<Integer>();
	
	
	public F77PROTODeclaration() {
		/** Initialization of intrinsic functions **/
		List<String> intr = Arrays.asList("IFIX","IDINT","FLOAT","SNGL","ICHAR","CHAR","DINT","DNINT","IDNINT","IABS","DABS","CABS","AMOD","DMOD","ISIGN","DSIGN","IDIM","DDIM","DPROD","MAX0","AMAX1","DMAX1","AMAX0","MAX1","MIN0","AMIN1","DMIN1","AMIN0","MIN1","DSQRT","CSQRT","DEXP","CEXP","ALOG","DLOG","CLOG","ALOG10","DLOG10","DSIN","CSIN","DCOS","CCOS","DTAN","DASIN","DACOS","DATAN","DATAN2","DSINH","DCOSH","DTANH");
		intrinsequesGeneriques.addAll(intr);
		List<String> intrGene = Arrays.asList("INT","REAL","DBLE","CMPLX","AINT","ANINT","NINT","ABS","MOD","SIGN","DIM","CMPLX","MAX","MIN","SQRT","EXP","LOG","LOG10","SIN","COS","TAN","ASIN","ACOS","ATAN","ATAN2","SINH","COSH","TANH");
		intrinsequesGeneriques.addAll(intrGene);
		
    }

	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	/** For each value of call functions see if the is a true error. An error is when the function called is
	    - not external
	    - not intrinsic
	    - not a internal functions in the same file
	    - not a variable defined in the same file
	**/ 
	private void compareLists() throws JFlexException {
		for (int i = 0; i < callsFunctions.size(); i++) {
			String func = callsFunctions.get(i);
			func = func.substring(0, func.length()-1);
			if (!externalFunctions.contains(func) && !internalFunctions.contains(func)) {
				setError(errorLocation.get(i),"The function " + func + " shall be declared.",errorLine.get(i));
			}
		}
		externalFunctions.clear();
		internalFunctions.clear();
		callsFunctions.clear();
		errorLocation.clear();
		errorLine.clear();
	}
%}

%eofval{
	compareLists();
    return getViolations();
%eofval}

%%          

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{FUNC}			{location = yytext(); yybegin(FUNCTION);}
<YYINITIAL>		{TYPE}        	{location = yytext();yybegin(NAMING);}
<YYINITIAL>		\=				{yybegin(RIGHT_SIDE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>		{FUNC}			{location = yytext(); yybegin(FUNCTION);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECLARATION);}
<NEW_LINE>		{EXTERNAL}		{yybegin(EXTERNAL);}
<NEW_LINE>		\=				{yybegin(RIGHT_SIDE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{yybegin(LINE);}
<LINE>			{FUNC}			{location = yytext(); yybegin(FUNCTION);}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{DATA_TYPE}		{yybegin(DECLARATION);}
<LINE>			{EXTERNAL}		{yybegin(EXTERNAL);}
<LINE>			\=				{yybegin(RIGHT_SIDE);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<FUNCTION>		{VAR}			{location = location + " " + yytext(); internalFunctions.add(yytext().toUpperCase()); yybegin(LINE);}
<FUNCTION>		\n				{yybegin(NEW_LINE);}
<FUNCTION>		.				{}

<RIGHT_SIDE>	{STRING}		{}
<RIGHT_SIDE>	{VAR}[\ ]*\(	{String var = yytext().toUpperCase().substring(0, yytext().length() -1).trim();
								 if(!variables.contains(var) && !intrinsequesGeneriques.contains(var)) { 
								 	callsFunctions.add(yytext().toUpperCase()); errorLocation.add(location); errorLine.add(yyline+1);
								 }}
<RIGHT_SIDE>	\n[\ ]{1,5}{SIMBOL}	{}
<RIGHT_SIDE>	\n				{yybegin(NEW_LINE);}
<RIGHT_SIDE>	.				{}

<DECLARATION>	{FUNC}			{location = yytext(); yybegin(FUNCTION);}
<DECLARATION>	{TYPE}        	{location = yytext(); yybegin(NAMING);}
<DECLARATION>	{VAR}			{variables.add(yytext().toUpperCase());}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}
<DECLARATION>	\n				{yybegin(NEW_LINE);}
<DECLARATION>	.				{}


<EXTERNAL>		{VAR}			{externalFunctions.add(yytext().toUpperCase());}
<EXTERNAL>		\n[\ ]{1,5}{SIMBOL}	{}
<EXTERNAL>		\n				{yybegin(NEW_LINE);}
<EXTERNAL>		.				{}


/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}

