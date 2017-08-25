/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Recursion rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class COMFLOWRecursion
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
TYPE		 = "function" | "procedure" | "subroutine" | "program" | "module" 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	
	public COMFLOWRecursion() {
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

/* Transition word is recursive (or RECURSIVE). This word must not be found. */
/* Whenever it's found, an error is returned.						 		 */
RULE_WORD = [^a-zA-Z0-9\_]"recursive"[^a-zA-Z0-9\_]

%%          
				{COMMENT_WORD}	{yybegin(COMMENT);}
				{STRING}		{}
				{STRING}		{}  

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>		{FALSE}			{}
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{RULE_WORD}		{this.setError(location,"The use of recursivity is not allowed.", yyline + 1);
								 yybegin(LINE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>		{FALSE}			{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{RULE_WORD}		{this.setError(location,"The use of recursivity is not allowed.", yyline + 1); 
			   					 yybegin(LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{FALSE}			{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE> 			{RULE_WORD}		{this.setError(location,"The use of recursivity is not allowed.", yyline + 1);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }