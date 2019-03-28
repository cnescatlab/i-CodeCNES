/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/****************************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.BooleanExpression rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	      		*/
/* As many comments have been done on the ExampleRule.lex file, this file         		*/
/* will restrain its comments on modifications.								     		*/
/*																			      		*/
/****************************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMFLOWBooleanExpression
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CONDITIONAL, LOOP

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

IF			 = "if" | "elif"
THEN		 = "then"
WHILE		 = "while" | "until"
DO			 = "do" 
BOOL		 = \|\|		| \&\&	| \-"o"	| \-"a"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	int booleanExpressions = 0;

    public COMFLOWBooleanExpression() {
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
			    {IF}			{booleanExpressions=0; yybegin(CONDITIONAL);}
			    {WHILE}			{booleanExpressions=0; yybegin(LOOP);}
			    {VAR}			{} /* Clause to match with words that contains "kill" */
			 	[^]            	{}
		}
		
/************************/
/* CONDITIONAL STATE    */
/************************/
<CONDITIONAL>   	
		{
				{BOOL}			{booleanExpressions++;
								 if (booleanExpressions==5) setError(location,"It is not allowed use five or more conditional expressions in the same instruction.", yyline+1);}
				\; | {THEN}		{booleanExpressions=0; yybegin(YYINITIAL);}
				{VAR}			{}
				[^]         	{}  
		}
		
/************************/
/* LOOP STATE	    */
/************************/
<LOOP>   	
		{
				{BOOL}			{booleanExpressions++;
								 if (booleanExpressions==5) setError(location,"It is not allowed use five or more conditional expressions in the same instruction.", yyline+1);}
				\; | {DO}		{booleanExpressions=0; yybegin(YYINITIAL);}
				{VAR}			{}
				[^]         	{}  
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}