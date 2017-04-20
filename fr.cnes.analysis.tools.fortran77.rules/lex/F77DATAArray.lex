/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.DATA.Array rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77DATAArray
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, PARS, DECLARATION

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision  | DIMENSION | dimension
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+ 
SPACE		 = [\ \r\t]
STRING		 = \'[^\']*\' | \"[^\"]*\"
SMB 		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]

																
%{
	String location = "MAIN PROGRAM";
	/** An star appears in the array declaration **/
	boolean star = false;
	/** The variable is an array **/
	boolean dimension = false;
	/** Number of brackets **/
	int pars = 0;
	/** variable name to analyze **/
	String variable = "";
	/** boolean to know if the comment line exist **/
	boolean comment = false; 
	
	public F77DATAArray() {
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



%%          

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{comment=true; yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{comment = false; yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/** Looking for a declaration and initialization **/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{comment = false; yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECLARATION);}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>		.				{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>      	\n             	{comment = false; yybegin(NEW_LINE);}
<LINE>      	.              	{}

/** Analyze the declaration of the array **/
<DECLARATION>	{VAR}[\ ]*\(	{pars=1; variable = yytext().substring(0, yytext().length()-1).trim(); yybegin(PARS);}
<DECLARATION>	\n[\ ]{1,5}{SMB} {}
<DECLARATION>	\n				{comment = false; yybegin(NEW_LINE);}
<DECLARATION>	.				{}

/** Check the content of the array: 
    - if a star appears that must be at the end -> A(*), A(4, *), A(4, *, *), A(4, 4, *) 
    - if a star appears and later on there is a number/variable throw an error -> A(5, *)
    - check brackets to analyse the beginning and end of the matrix declaration
**/
<PARS>			{INT}|{VAR}		{if(star && dimension) {
									setError(location,"The dimension of the array "+ variable + " is not well declared. The * shall be used for the last dimension.", yyline+1);
									}
								}
<PARS>			\*[\ ]*\,		{star = true; 
								 dimension = true;}
<PARS>			\*[\ ]*\)		{star = true; pars--;
								 if(pars==0) {
								 	if (!comment) {
								 	setError(location,"The dimension of the array " + variable + " is not well declared. A comment who justifies the use of * is needed before. ", yyline+1);
								 	}
								 	star=false; dimension=false;
								 	yybegin(DECLARATION);
								 }
								}
<PARS>			\(				{pars++;}
<PARS>			\)				{pars--;
								 if(pars==0) {
								 	star=false; dimension=false;
								 	yybegin(DECLARATION);
								 }
								}
<PARS>			\n[\ ]{1,5}{SMB} {}
<PARS>			\n				{comment = false; star = false; yybegin(NEW_LINE);}
<PARS>			.				{}	


				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
