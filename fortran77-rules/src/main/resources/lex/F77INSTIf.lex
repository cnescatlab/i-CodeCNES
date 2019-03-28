/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.INST.If rule.  		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;

%%

/* Column counting is set to help find line continuation. */
%class F77INSTIf
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state is added to check if a THEN follows an IF. 								 */
/* A state called BRAKET is set to determine parenthesis part after an IF statement. */
/* A state called COMA is used to determine if coma is found after an IF statement,  */
/* which means an arithmetical if is used.											 */ 
%state COMMENT, NAMING, NEW_LINE, LINE, IF, BRAKET, COMA

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
INT			 = [0-9]+

%{
	String location = "MAIN PROGRAM"; 
	/** name of the file parsed */
	private String parsedFileName;
	
	int bracket = 0;
	
	public F77INSTIf() {
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

/* An integer called LAST_STATE is used to avoid error due to line continuation. */
%{
	int LAST_STATE = LINE;
%}

/* Transition word are if (or IF) and then (or THEN). */
RULE_WORD = if   | IF
END		  = end  | END
THEN	  = THEN | then
CONT	  = CONTINUE  | continue

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
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/*****************/
/*	 IF STATE	 */
/*****************/
<IF>			\(				{bracket=1; yybegin(BRAKET);}
<IF>			.				{}
<IF>			\n				{LAST_STATE = IF;
								 yybegin(NEW_LINE);}
								 
/*********************/
/*	 BRAKET STATE	 */
/*********************/
<BRAKET>		\(				{bracket++;}
<BRAKET>		\)				{bracket--; if(bracket==0) yybegin(COMA);}
<BRAKET>		[^]			{}
								 
/****************/
/*  COMA STATE	*/
/****************/
<COMA>			{THEN}			{}
<COMA>			{INT}[\ ]*{CONT} {}
<COMA>			{INT}			{setError(location,"The arithmetic if is not allowed.", yyline + 1);
								 yybegin(COMMENT);}
<COMA>			{VAR}			{yybegin(COMMENT);}
<COMA>			\n [\ ]* {COMMENT_WORD}	{yybegin(COMMENT);}			
<COMA>			\n				{}
<COMA>			.				{}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/*********************/
/*	NEW LINE STATE	 */
/*********************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{END}			{yybegin(COMMENT);}
<LINE> 			{RULE_WORD}		{yybegin(IF);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{LAST_STATE = LINE;
								 yybegin(NEW_LINE);}
<LINE>			[^ \t]			{if (yycolumn == 5) yybegin(LAST_STATE);}
<LINE>      	.              	{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}