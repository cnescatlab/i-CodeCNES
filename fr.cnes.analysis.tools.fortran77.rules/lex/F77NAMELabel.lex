/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.NAME.Label rule.	 */
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

%class F77NAMELabel
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* Two states are added:
 */
%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
ALLOW		 = FORMAT	  | format	|
			   CONTINUE	  | continue	
LABEL_ALLOW	 = [0-9]{4}[\ ]+{ALLOW}
LABEL_ERROR  = [0-9]{4}[\ ]+{VAR}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM"; 
	/** name of the file parsed */
	private String parsedFileName;
	
	
	public F77NAMELabel() {
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

<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{LABEL_ALLOW}	{}
<YYINITIAL>		{LABEL_ERROR}	{setError(location,"The use of labels is not allowed except with the instructions FORMAT and CONTINUE.",yyline+1);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{LABEL_ALLOW}	{}
<NEW_LINE>		{LABEL_ERROR}	{setError(location ,"The use of labels is not allowed except with the instructions FORMAT and CONTINUE.",yyline+1);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{yybegin(LINE);}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{LABEL_ALLOW}	{}
<LINE>			{LABEL_ERROR}	{setError(location,"The use of labels is not allowed except with the instructions FORMAT and CONTINUE.",yyline+1);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
				               }
