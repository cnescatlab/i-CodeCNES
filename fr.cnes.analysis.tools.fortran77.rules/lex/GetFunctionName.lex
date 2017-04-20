/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Tr.IfElse rule.			 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class GetFunctionName
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>

/* A state is added:
 * FUNCTION_NAME: save the name when a FUNCTION is found
 */
%state COMMENT, NAMING, NEW_LINE, LINE, FUNCTION_NAME

COMMENT_WORD = \!         | c          | C
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
	
	List<String> functionName = new LinkedList<String>();
	
	public GetFunctionName() {
    }
    
    public List<String> getFunctionName() {
    	return functionName;
    }

	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
%}

%eofval{
return getViolations();
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
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>		{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<NEW_LINE>  	\n             	{yybegin(NEW_LINE);}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

/* In the state FUNCION_NAME we must save the name of the function in the linkedist*/
<FUNCTION_NAME>	{VAR}			{functionName.add(yytext());
								 yybegin(COMMENT);}
<FUNCTION_NAME>	\n				{yybegin(NEW_LINE);}
<FUNCTION_NAME>	.				{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}