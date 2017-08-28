/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Abort rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWAbort
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, KILL

COMMENT_WORD = \#
FUNC         = "function"
VAR		     = (\$)?[a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

ABORT		= ("kill"|"pkill"|"killall")
OPTIONS		= \- ("9" | "SIGKILL" | "kill")

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	String errorKill = "";

    public COMFLOWAbort() {
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



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = location + yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNC}        	{location = yytext(); yybegin(NAMING);}
			    {ABORT}			{errorKill= yytext(); yybegin(KILL);}
			    {STRING}		{}
			    {VAR}			{} /* Clause to match with words that contains "kill" */
			 	[^]            	{}
		}
	
/************************/
/* KILL STATE	    */
/************************/	
<KILL>
		{
				{OPTIONS}		{setError(location,"The keyword " + errorKill + " is not allowed.", yyline+1); yybegin(COMMENT);}
				{VAR}			{setError(location,"The keyword " + errorKill + " is not allowed.", yyline+1); yybegin(COMMENT);}
				\-{VAR}			{yybegin(COMMENT);}
				\n 				{yybegin(YYINITIAL);}
				.				{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}