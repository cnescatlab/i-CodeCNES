/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for F77.Data.Initialisation rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 	  */
/* As many comments have been done on the ExampleRule.lex file, this file    	  */
/* will restrain its comments on modifications.								 	  */
/*																			 	  */
/**********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77DATAInitialisation
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, CMN_STATE, INIT

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
COMMON		 = COMMON     | common
DATA		 = DATA		  | data
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
VAR_EQ		 = {VAR} ([\ ]* \( [^\)]* \) )? [\ ]* \= 
STRING		 = \'[^\']*\' | \"[^\"]*\"
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]
																
%{
	String location = "MAIN PROGRAM";
	
	List<String> identifiers = new LinkedList<String>();
	
	public F77DATAInitialisation() {
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

<NAMING>		{VAR}			{identifiers.clear(); location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{identifiers.clear(); yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>		<STRING>		{}
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{COMMON}		{yybegin(CMN_STATE);}
<YYINITIAL>		{DATA}			{yybegin(INIT);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{COMMON}		{yybegin(CMN_STATE);}
<NEW_LINE>		{DATA}			{yybegin(INIT);}
<NEW_LINE>		{VAR_EQ}		{String var = yytext().replaceAll("\\s","").replaceAll("\\=", "");
								 if(var.contains("(")) var = var.split("\\(")[0];
								 if(identifiers.contains(var)) {
								 	setError(location,"The variable "+var+" shall be initialized with DATA or BLOCK DATA before its use.", yyline+1); 
								 	identifiers.remove(var); } }
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{COMMON}		{yybegin(CMN_STATE);}
<LINE>			{DATA}			{yybegin(INIT);}
<LINE>			{VAR_EQ}		{String var = yytext().replaceAll("\\s","").replaceAll("\\=", "");
								 if(var.contains("(")) var = var.split("\\(")[0];
								 if(identifiers.contains(var)) { 
								 	setError(location,"The variable "+var+" shall be initialized with DATA or BLOCK DATA before its use." + var, yyline+1); 
								 	identifiers.remove(var); } }
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<CMN_STATE>		\/{VAR}\/		{}
<CMN_STATE>		{VAR}			{identifiers.add(yytext());}
<CMN_STATE>		\n				{yybegin(NEW_LINE);}
<CMN_STATE>		.				{}

<INIT>			{VAR}			{if(identifiers.contains(yytext())) identifiers.remove(yytext());}
<INIT>			\/				{yybegin(COMMENT);}
<INIT>			\n[\ ]{1,5}{SIMBOL}	{}
<INIT>			\n				{yybegin(NEW_LINE);}
<INIT>			.				{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
