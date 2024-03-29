/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.BLOC.Common rule.    */
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

import java.util.logging.Logger;

import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;


%%

%class F77BLOCCommon
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

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
STRING		 = \'[^\']*\' | \"[^\"]*\"

%{
    private static final Logger LOGGER = Logger.getLogger(F77BLOCCommon.class.getName());

	String location = "MAIN PROGRAM"; 
    String parsedFileName;
	
	
	public F77BLOCCommon() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
        LOGGER.finest("end method setInputFile");
	}
%}


%eofval{
return getCheckResults();
%eofval}
%eofclose


/* Transition word is common (or COMMON). If COMMON is found without a	*/
/* backslash after it, it means it's a blank common, thus an error is 	*/
/* sent. This do not work if COMMON line is the last one of the file,	*/
/* which can't happen.													*/
COMMON    = common		| COMMON
RULE_WORD = {COMMON}([^\/\n]*)([\n])

%%             
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/
				{FREE_COMMENT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
				                    yybegin(COMMENT);}

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);} 
<COMMENT>   	.              	{}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		{VAR}			{location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext();
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								 yybegin(NAMING);}
<YYINITIAL>		{RULE_WORD}		{
                                    LOGGER.fine("Setting error line "+(yyline+1)+" because of the unnamed COMMON is not allowed..");
                                    this.setError(location,"Unnamed COMMON is not allowed.", yyline + 1);
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : RULE_WORD \""+yytext()+"\" )");
								    yybegin(NEW_LINE);}
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);}

/*********************/
/*	NEW LINE STATE	 */
/*********************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);}
<NEW_LINE>		{TYPE}        	{location = yytext();
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<NEW_LINE>		{RULE_WORD}		{
                                    LOGGER.fine("Setting error line "+(yyline+1)+" because of the unnamed COMMON is not allowed..");
                                    this.setError(location,"Unnamed COMMON is not allowed.", yyline + 1);
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NEW_LINE (Transition : RULE_WORD \""+yytext()+"\" )");
								    yybegin(NEW_LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext();
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<LINE> 			{RULE_WORD}		{
                                    LOGGER.fine("Setting error line "+(yyline+1)+" because of the unnamed COMMON is not allowed..");
                                    this.setError(location,"Unnamed COMMON is not allowed.", yyline + 1);
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : RULE_WORD \""+yytext()+"\" )");
								    yybegin(NEW_LINE);
							    }
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]           {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                               }