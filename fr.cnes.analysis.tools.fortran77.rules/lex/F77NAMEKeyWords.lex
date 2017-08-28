/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77NAMEKeyWords rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77NAMEKeyWords
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* Two states are added:
 */
%state COMMENT, NAMING, NEW_LINE, LINE, CLE

COMMENT_WORD = \!         | c          | C		| \*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
MOT_CLE		 = ASSIGN | assign | BACKSPACE | backspace | BLOCK | block | DATA | data |
               CALL | call | CLOSE | close | COMMON | common | CONTINUE | continue |
               DATA |data | DIMENSION | dimension | DO |do | ELSE | else | ELSE IF |
               else if | END | end | ENDFILE | endfile | ENDIF |endif | ENTRY | entry |
               EQUIVALENCE | equivalence | EXTERNAL | external | FORMAT | format | 
               FUNCTION | function | GOTO | goto | IF | if | IMPLICIT | implicit |
               INQUIRE | inquire | INTEGER | integer | INTRINSIC | intrinsic | OPEN | open | PARAMETER |
               parameter | PAUSE | pause | PRINT | print | PROGRAM | program | READ |
               read | RETURN | return | REWIND | rewind | REWRITE | rewrite | SAVE |
               save | STOP | stop | SUBROUTINE | subroutine | THEN | then | WRITE | write
VARIABLE	 = INTEGER | integer | REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
			   double[\ ]+precision | CHARACTER | character 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\r]
																
%{
	String location = "MAIN PROGRAM"; 
	/** name of the file parsed */
	private String parsedFileName;
	
	
	public F77NAMEKeyWords() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
%}

%eofval{
    return getCheckResults();
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
<YYINITIAL>		{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<YYINITIAL>		{VARIABLE}		{yybegin(CLE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>		{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<NEW_LINE>		{VARIABLE}		{yybegin(CLE);}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{yybegin(LINE);}
<LINE>			{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<CLE>			{TYPE}			{location = yytext(); yybegin(NAMING);}
<CLE>			{MOT_CLE}		{this.setError(location,"The variable " + yytext() + " is a keyword in Fortran77 language.", yyline + 1);}
<CLE>			{VAR}			{}
<CLE>			\n				{yybegin(NEW_LINE);}
<CLE>			.				{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}
