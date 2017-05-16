/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.DATA.LoopDO rule. 	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;


%%

/* Column counting is used to deal with line completion problems. */
%class F77DATALoopDO
%extends AbstractRule
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<Violation>

/* A state called ENTER_DO is created. It allows to determine the condition	*/
/* of a DO-loop and certify that it's not a WHILE-loop.						*/
/* A state called INDEX is to determine whenever the equal sign is passed.	*/
/* A state called INIT is set to get all declared variables that are not an */
/* integer.																	*/
%state COMMENT, NAMING, NEW_LINE, LINE, ENTER_DO, INDEX, INIT, PAR, PARI

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\r]

%{
	String location = "MAIN PROGRAM";
	
	
	public F77DATALoopDO() {
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

/* A boolean called error is to determine if a non integer value is found. */
/* A list of String will contain name of declared variables.			   */
/* An integer called LAST_STATE is used when line completion is found.	   */
%{
	boolean violation = false;
	List<String> wrongTypeVariables = new LinkedList<String>();
	int LAST_STATE = LINE;
	int par = 0;
	String variable = "";
%}

/* Transition word is do (or DO). If WHILE is found, nothing has to be done. */
/* Words are set to identify undesired types, such as real or complex.		 */
RULE_WORD = do    | DO
WHILE	  = while | WHILE
END		  = end	  | END

REAL        = real 				| REAL
DOUBLE_PREC = double[\ ]+precision 	| DOUBLE[\ ]+PRECISION
COMPLEX     = complex			| COMPLEX
LOGICAL		= logical			| LOGICAL
CHAR		= character			| CHARACTER
WRONG_TYPE  = {REAL} 			| {DOUBLE_PREC} | {COMPLEX} | {LOGICAL} | {CHAR}
%%          
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/
				{FREE_COMMENT}	{yybegin(COMMENT);}

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		{VAR}			{location = location + " " + yytext();
								 wrongTypeVariables.clear();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{wrongTypeVariables.clear();
								 yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/*****************************/
/*	INITIALIZATION STATE	 */
/*****************************/
/* Whenever a type declaration is found, we check that only one variable is on the */
/* line, which there is no comma (,).											   */ 
<INIT> 			{VAR}			{wrongTypeVariables.add(yytext());}
<INIT>			\(				{par=1; yybegin(PARI);}
<INIT>			.				{}
<INIT>			\n				{LAST_STATE = INIT;
								 yybegin(NEW_LINE);}
								 
<PARI>			\(				{par++;}
<PARI>			\)				{par--; if(par==0) yybegin(INIT);}
<PARI>			\n				{yybegin(LAST_STATE);}
<PARI>			.				{}

/*****************************/
/*	 DO DECLARATION STATE	 */
/*****************************/
/* If an equal sign appears, we go straight to INDEX part. */
<ENTER_DO>		{WHILE}			{yybegin(COMMENT);}
<ENTER_DO>		{VAR}			{if (wrongTypeVariables.contains(yytext())) { violation = true; variable = variable + " " + yytext(); }}
<ENTER_DO>		\=				{yybegin(INDEX);}
<ENTER_DO>		.				{}
<ENTER_DO>		\n				{violation=false; yybegin(NEW_LINE);}

/*****************************/
/*	 INDEX CHECKING STATE	 */
/*****************************/
/* We do nothing for integer, blank and end of line. Otherwise, an error is set. */
<INDEX>			[0-9]+			{}
<INDEX>			{VAR}			{if (wrongTypeVariables.contains(yytext())) { violation = true; variable = variable + " " + yytext(); }}
<INDEX>			\(				{par=1; yybegin(PAR);}
<INDEX>			\,				{}
<INDEX>			[0-9]*\.[0-9]+	{violation = true;  variable = variable + " " + yytext();}
<INDEX> 		.				{}
<INDEX>			\n				{if (violation) this.setError(location,"The control variable in a loop shall be an integer.", yyline + 1);
								 violation = false; variable = "";
								 yybegin(NEW_LINE);}
								 
<PAR>			\(				{par++;}
<PAR>			\)				{par--; if(par==0) yybegin(INDEX);}
<PAR>			\n				{yybegin(NEW_LINE);}
<PAR>			.				{}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<YYINITIAL>		{WRONG_TYPE}	{yybegin(INIT);}
<YYINITIAL>		{RULE_WORD}		{yybegin(ENTER_DO);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/*********************/
/*	NEW LINE STATE	 */
/*********************/	
/* If END is found, we check whether it corresponds to a DO-loop. If it is	*/
/* we remove the last condition word. Then, we remove last indentifier.		*/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<NEW_LINE>		{WRONG_TYPE}	{yybegin(INIT);}
<NEW_LINE>		{RULE_WORD}		{yybegin(ENTER_DO);}
<NEW_LINE>		{END}			{yybegin(COMMENT);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<LINE>			{RULE_WORD}		{yybegin(ENTER_DO);}
<LINE>			{END}			{yybegin(COMMENT);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>			.				{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
