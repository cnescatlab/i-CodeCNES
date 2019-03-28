/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.FLOW.CheckArguments rule.  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class SHFLOWCheckArguments
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CHECKARGUMENTS, COMMENT_FUNCTION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

COMP		 = "-eq"	| "-ne"		| "-gt"		| "-ge"		|
			   "-lt"	| "-le"		| \<		| \<\=		|
			   \>		| \>\=
ARGS		 = "if"{SPACE}+\[{SPACE}+{NUMBER_PARAMS}{SPACE}+{COMP}
NUMBER_PARAMS = \$\# | \$\{\#\}
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	int lastFunctionLine = 0;

    public SHFLOWCheckArguments() {
    	
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
		
<COMMENT_FUNCTION>   	
		{
				\n             	{yybegin(CHECKARGUMENTS);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); yybegin(CHECKARGUMENTS);}
				\n             	{yybegin(CHECKARGUMENTS);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			  	{STRING}		{}
				{FUNCTION}     	{lastFunctionLine=yyline+1; yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); lastFunctionLine=yyline+1; yybegin(CHECKARGUMENTS);}
	      		[^]         	{}
		}
		
		
/************************/
/* CHECKARGUMENTS STATE	*/
/************************/
<CHECKARGUMENTS>   	
		{
				{ARGS}				{yybegin(YYINITIAL);}
				{STRING}			{}
				{SPACE} | \{ | \n	{}
				{COMMENT_WORD} 		{yybegin(COMMENT_FUNCTION);}
			   	.              		{setError(location,"The number of parameters received has not been checked.", lastFunctionLine); yybegin(YYINITIAL);}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}