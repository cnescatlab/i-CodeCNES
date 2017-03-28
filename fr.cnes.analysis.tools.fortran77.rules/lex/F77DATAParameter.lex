/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Tr.Parametres rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77DATAParameter
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These states are added:
 - DECLARATION:archive the values of the variables in a list
 - FUNCTION: when a function is called stays in this state until the parameters will be found
 - PARAMETERS: check the function parameters
 */
%state COMMENT, NAMING, NEW_LINE, LINE, DO_LINE, FUNCTION, PARAMETERS, DECLARATION, PAR

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
CALL		 = CALL       | call
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
LOOPS		 = else	| ELSE | do | DO | end | END | if | IF |then | THEN
CLE			 = {LOOPS} | {DATA_TYPE} | {TYPE}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
NUM			 = (\-)? ([0-9]+ | [0-9]+\.[0-9]* | [0-9]*\.[0-9]+(("E"|"D")(\-)?[0-9+])?)
OP			 = \+ | \- | \* | \/ | \*\*
EXPR		 = ({VAR}|{NUM}) ({OP} ({VAR}|{NUM}) )+
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f]
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]	| \*
																
%{
	String location = "MAIN PROGRAM";
	/** List of variables that causes an error what they are called inside a function **/
	List<String> identifiers = new ArrayList<String>();
	/** Number of pair of brackets **/
	int par = 0;
	/** Paramters to throw error **/
	String errors = "";
	boolean first = true;
	
	
	public F77DATAParameter() {
    }

	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	private void checkFucntionParameter(String param) throws JFlexException {
		if (!identifiers.contains(param)) {
			if(first) {
				errors = "Error in the following parameters: " + param;
				first = false;
			}
			else errors = errors + ", " + param;
		}
	}
	
%}

%eofval{
return getViolations();
%eofval}


%%          

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{CALL}			{yybegin(FUNCTION);}
<YYINITIAL>		{CLE}			{}
<YYINITIAL>		{VAR}			{if(!identifiers.contains(yytext())) identifiers.add(yytext());}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECLARATION);}
<NEW_LINE>		{CALL}			{yybegin(FUNCTION);}
<NEW_LINE>		{CLE}			{yybegin(LINE);}
<NEW_LINE>		{VAR}			{if(!identifiers.contains(yytext())) identifiers.add(yytext()); yybegin(LINE);}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{CALL}			{yybegin(FUNCTION);}
<LINE>			{CLE} 			{}
<LINE>			{VAR}			{if(!identifiers.contains(yytext())) identifiers.add(yytext());}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}[\ ]* \(	{String var = yytext().substring(0, yytext().length()-1).trim();  
								 identifiers.add(var);}
<DECLARATION>	{VAR}			{}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}			
<DECLARATION>  	\n             	{yybegin(NEW_LINE);}
<DECLARATION>  	.              	{}


<FUNCTION>		\(				{yybegin(PARAMETERS);}
<FUNCTION>		{VAR}			{}
<FUNCTION>		\n				{yybegin(NEW_LINE);}
<FUNCTION>		. 				{}

<PARAMETERS>	{EXPR}|{NUM} | {STRING}	{if(first) { errors = "Error in the following parameters: " + yytext(); first = false; }
										 else errors = errors + ", " + yytext();}
<PARAMETERS>	{VAR}[\ ]*\(			{String var = yytext().substring(0, yytext().length()-1).trim();
								 		 checkFucntionParameter(var); par = 1; yybegin(PAR);}
<PARAMETERS>	[0-9]*{VAR}				{}
<PARAMETERS>	\n[\ ]{1,5}{SIMBOL}		{if(!errors.isEmpty()) setError(location,errors+" belongs to parameter types forbidden when calling a function: a constant, an expression to be evaluated, a call to another function", yyline+1); errors = ""; first = true;}
<PARAMETERS>	\n						{if(!errors.isEmpty()) setError(location,errors+" variable belongs to parameter types forbidden when calling a function: a constant, an expression to be evaluated, a call to another function", yyline+1); errors = ""; first = true; yybegin(NEW_LINE);}
<PARAMETERS>	.						{}

<PAR>			\(				{par++;}
<PAR>			\)				{par--; if(par==0) yybegin(PARAMETERS); }
<PAR>			[^]			{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}