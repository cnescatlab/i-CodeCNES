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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77DATAIO
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, IO_STATE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
IO			 = READ       | read       |
			   WRITE	  | write      |
			   PRINT      | print
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

																
%{
	String location = "MAIN PROGRAM";
	
	String io;
	
	public F77DATAIO() {
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

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{IO}			{io = yytext().toUpperCase(); yybegin(IO_STATE);}
<YYINITIAL>		{VAR}			{}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{IO}			{io = yytext().toUpperCase(); yybegin(IO_STATE);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{IO}			{io = yytext().toUpperCase(); yybegin(IO_STATE);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<IO_STATE>		\( [\ ]* \*		{if(io.equals("READ")||io.equals("WRITE")) setError(location,"The use of * with logical units is not allowed.", yyline+1); yybegin(LINE);}
<IO_STATE>		\*				{if(io.equals("READ")) setError(location,"The use of * with logical units is not allowed.", yyline+1); yybegin(LINE);}
<IO_STATE>		{VAR}			{if(io.equals("PRINT")) setError(location ,"The use of * with logical units is not allowed.", yyline+1); yybegin(LINE);}
<IO_STATE>		\n				{yybegin(NEW_LINE);}
<IO_STATE>		.				{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
