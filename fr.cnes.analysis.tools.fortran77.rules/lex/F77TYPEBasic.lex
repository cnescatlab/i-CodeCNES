/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.TYPE.Basic rule.	 */
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

%class F77TYPEBasic
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, RD_WR, RD_WR_PARAM

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE_ERROR	 = INTEGER\*[0-9]+ |integer\*[0-9]+ | LOGICAL\*[0-9]+ | 
			   logical\*[0-9]+ | CHARACTER\*[0-9]+ | character\*[0-9]+ |
               REAL\*[0-9]+ | real\*[0-9]+ | COMPLEX\*[0-9]+ | complex\*[0-9]+ | 
               DOUBLE[\ ]+PRECISION\*[0-9]+ | double[\ ]+precision\*[0-9]+
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\r]
																
%{
	String location = "MAIN PROGRAM"; 
	/** name of the file parsed */
	private String parsedFileName;
	
	
	public F77TYPEBasic() {
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

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>  	{COMMENT_WORD} 		{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        		{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{DATA_TYPE_ERROR}	{setError(location , yytext() + " is not a basic type. Basic types are INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL and CHARACTER.", yyline+1); 
									 yybegin(LINE);}
<YYINITIAL>		{SPACE}				{}
<YYINITIAL> 	\n             		{yybegin(NEW_LINE);}
<YYINITIAL> 	.              		{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 		{yybegin(COMMENT);}
<NEW_LINE>		{STRING}			{yybegin(LINE);}
<NEW_LINE>		{TYPE}        		{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE_ERROR}	{setError(location, yytext() + " is not a basic type. Basic types are INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL and CHARACTER.", yyline+1); 
									 yybegin(LINE);}
<NEW_LINE>		{SPACE}				{}
<NEW_LINE>  	\n             		{}
<NEW_LINE>  	.              		{yybegin(LINE);}

<LINE>			{STRING}			{}
<LINE>			{TYPE}        		{location = yytext(); yybegin(NAMING);}
<LINE>      	\n             		{yybegin(NEW_LINE);}
<LINE>      	.              		{}

				[^]            	{
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
				               }