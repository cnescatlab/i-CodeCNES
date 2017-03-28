/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.REF.Parameter rule.	 */
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

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77REFParameter
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, CMN_STATE, FUNCTION, FNC_PARS

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
COMMON		 = COMMON       | common
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
CALL		 = CALL       | call
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
PARAM		 = [^\,\)\n\ ] 
																
%{
	String location = "MAIN PROGRAM";
	
	List<String> commonList = new LinkedList<String>();
	
	public F77REFParameter() {
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
<YYINITIAL>		{STRING}		{}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{COMMON}		{yybegin(CMN_STATE);}
<YYINITIAL>		{CALL}			{yybegin(FUNCTION);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{COMMON}		{yybegin(CMN_STATE);}
<NEW_LINE>		{CALL}			{yybegin(FUNCTION);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{COMMON}		{yybegin(CMN_STATE);}
<LINE>			{CALL}			{yybegin(FUNCTION);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}
		
<CMN_STATE>		\/{VAR}\/		{}
<CMN_STATE>		\,				{}
<CMN_STATE>		{VAR}			{commonList.add(yytext());}
<CMN_STATE>		\n				{yybegin(NEW_LINE);}
<CMN_STATE>		.				{}

<FUNCTION>		\(				{yybegin(FNC_PARS);}
<FUNCTION>		{VAR}			{}
<FUNCTION>		\n				{yybegin(NEW_LINE);}
<FUNCTION>		.				{}

<FNC_PARS>		{PARAM}			{if(commonList.contains(yytext())) setError(location,"It is not allowed to provide as a parameter the variables of an accessible bloc COMMON. "+
																			"The variable " + yytext() + " is used in a wrong way.",yyline+1);}
<FNC_PARS>		\)				{yybegin(FUNCTION);}
<FNC_PARS>		\n				{yybegin(NEW_LINE);}
<FNC_PARS>		.				{}


				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
