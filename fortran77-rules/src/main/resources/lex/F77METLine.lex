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

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F77METLine
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
																
%{
	String location = "MAIN PROGRAM"; 
	/** name of the file parsed */
	private String parsedFileName;
	
	int lineChars = 0;
	boolean showError = false;
	
	public F77METLine(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}

	
%}

%eofval{
return getCheckResults();
%eofval}
%eofclose

%%          

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{lineChars=0; yybegin(NEW_LINE);}
<NAMING>    	.              	{lineChars++;
								 if(lineChars>72 && yytext()!="&" ) {
								 	setError(location,"There are more than 72 characters in this line.",yyline);
								 	showError=true; 
								 }}

<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{lineChars+=yytext().length(); location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{lineChars=0; yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{lineChars++; yybegin(LINE);}


<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{lineChars=yytext().length(); location = yytext(); yybegin(NAMING);}
<NEW_LINE>  	\n             	{lineChars=0; showError=false;}
<NEW_LINE>  	.              	{lineChars=0; yybegin(LINE);}


<LINE>			{TYPE}        	{location = yytext(); lineChars+=yytext().length(); 
								 if(lineChars>72 && yytext()!="&" && !showError) {
								 	setError(location,"There are more than 72 characters in this line.",yyline);
								 	showError=true; 
								 }
								 yybegin(NAMING);}
<LINE>      	\n             	{lineChars=0; showError=false; yybegin(NEW_LINE);}
<LINE>      	.              	{lineChars++;
								 if(lineChars>72 && !yytext().equals("&") && !showError) {
								 	setError(location,"There are more than 72 characters in this line.",yyline+1);
								 	showError=true; 
								 }
								}

				[^]           {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}