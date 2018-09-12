/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.LoopCondition rule.  */
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

%class COMINSTLoopCondition
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, LOOPCONDITION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\'   | \"[^\"]*\"

EQUAL		 = "-eq"		| \=		| \=\=
DIFF		 = "-ne"		| \!\=	
INEG		 = 	\<			| \>		| \<\=		| \>\=		|
			    "-lt"		| "-gt"		| "-le"		| "-ge" 
COND_ERROR	 = {EQUAL}		| {DIFF}

/** The loop conditions are: WHILE, UNTIL 
  * The FOR loop is a little bit different from other programming languages. 
  * Basically, it let's you iterate over a series of 'words'  within a string. 
  **/
WHILE		 = "while"
UNTIL		 = "until"
LOOP		 = {WHILE}	| {UNTIL} 	
			   


																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;

    public COMINSTLoopCondition() {
    	
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
				{FNAME}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();}
			    {STRING}		{}
			    {LOOP}			{yybegin(LOOPCONDITION);}
	      		[^]            	{}
		}
		
/************************/
/* LOOPCONDITION STATE  */
/************************/
<LOOPCONDITION>
		{
			    {STRING}		{}
			    {INEG}			{}
			    {COND_ERROR}	{setError(location,"Inside a loop condition, it is not allowed equality (-eq, ==) or difference(-ne, !=) comparison." , yyline+1); yybegin(COMMENT);}
			    \; | \n			{yybegin(YYINITIAL);}
	      		.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}