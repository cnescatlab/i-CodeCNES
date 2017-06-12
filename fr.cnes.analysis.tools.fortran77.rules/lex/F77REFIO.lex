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
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77REFIO
%extends AbstractChecker
%public
%line

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
IO			 = READ       | read       |
			   WRITE	  | write
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+
																
%{
	String location = "MAIN PROGRAM";
	
	
	public F77REFIO() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
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

<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{IO}			{yybegin(RD_WR);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{IO}			{yybegin(RD_WR);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{IO}			{yybegin(RD_WR);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<RD_WR>			\(				{yybegin(RD_WR_PARAM);}
<RD_WR>			\n				{yybegin(NEW_LINE);}
<RD_WR>			.				{}

<RD_WR_PARAM>	\,				{yybegin(LINE);}
<RD_WR_PARAM>	{INT}			{setError(location,"The logical entities shall be declared using a symbolic name.", yyline+1);}
<RD_WR_PARAM>	{VAR}			{}
<RD_WR_PARAM>	\n				{yybegin(NEW_LINE);}
<RD_WR_PARAM>	.				{}
		

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}