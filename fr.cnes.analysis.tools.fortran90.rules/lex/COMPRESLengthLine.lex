/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.LengthLine rule.   */
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESLengthLine
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
																
%{
	String location = "MAIN PROGRAM";
	int chars = 1;

	
	public COMPRESLengthLine(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	

	
%}

%eofval{
    
	
	return getViolations();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{chars+=yytext().length(); yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{if(chars>100) setError(location,"There are more than 100 characters in this line.", yyline+1); chars=1; yybegin(NEW_LINE);}  
<COMMENT>   	.              	{chars+=yytext().length();}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{chars+=yytext().length(); location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{if(chars>100) setError(location,"There are more than 100 characters in this line.", yyline+1); chars=1; yybegin(NEW_LINE);}
<NAMING>    	.              	{chars+=yytext().length();}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{chars+=yytext().length(); yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{chars+=yytext().length(); location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{if(chars>100) setError(location,"There are more than 100 characters in this line.", yyline+1); chars=1; yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{chars+=yytext().length(); yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{chars+=yytext().length(); yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{chars+=yytext().length(); location = yytext(); yybegin(NAMING);}
<NEW_LINE>  	\n             	{if(chars>100) setError(location,"There are more than 100 characters in this line.", yyline+1); chars=1; }
<NEW_LINE>  	.              	{chars+=yytext().length(); yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{chars+=yytext().length(); location = yytext(); yybegin(NAMING);}
<LINE>      	\n             	{if(chars>100) setError(location,"There are more than 100 characters in this line.", yyline+1); chars=1; yybegin(NEW_LINE);}
<LINE>      	.              	{chars+=yytext().length();}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}