/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.BooleanExpression. */
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
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


%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \! 
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
LOGIC	     = \.AND\.	  | \.OR\.	   | \.NEQV\.		| \.XOR\.	|
			   \.EQV\.	  | \.NOT\.
RELAT		 = \.LT\.	  | \.LE\.	   | \.EQ\.			| \.NE\.	|
			   \.GT\.	  | \.GE\.     | \<             | \<\=      |
			   \=\=       | \/\=       | \>             | \>\=
OPERATOR     = {LOGIC}    | {RELAT} 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	int numExpr = 0;
	int numPar = 0;
	boolean endLine = true;
	int errorLine = 0;
	
	public COMFLOWBooleanExpression(){
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

/************************/

				{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{OPERATOR}		{if (errorLine == 0) errorLine = yyline + 1;
								 numExpr++;}
<NEW_LINE>		&				{endLine = false;}
<NEW_LINE>		\n				{if(endLine){
									if(numExpr > 5) 
										setError(location,"Using more than five conditions in an expression is not allowed.", errorLine);
									numExpr=0; errorLine = 0; 
								  }
								 endLine = true;}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{OPERATOR}		{if (errorLine == 0) errorLine = yyline + 1;
								 numExpr++;}
<LINE>			&				{endLine = false;}
<LINE>			\n				{if(endLine){
									if(numExpr > 5) 
										setError(location,"Using more than five conditions in an expression is not allowed.", errorLine);
									numExpr=0; errorLine = 0; yybegin(NEW_LINE);
								  }
								 endLine = true;}
<LINE>      	.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }