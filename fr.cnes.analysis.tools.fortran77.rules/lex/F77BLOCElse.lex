/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.BLOC.Else rule.		 */
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

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

/* We add column count to determine if a continuation of line is used. */
%class F77BLOCElse
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A new state called ELSE_LINE, is made to determine if ELSE is followed    */
/* by an IF word.															 */
/* A state called IFLINE is to determine whether the statement is stored. If */
/* it is followed by a THEN, we store it, else it's ignored.				 */
/* A state called ENDLINE is used to avoid END FILE statement.				 */
%state COMMENT, NAMING, NEW_LINE, LINE, ELSE_LINE, IFLINE, ENDLINE

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
    private static final Logger LOGGER = Logger.getLogger(F77BLOCElse.class.getName());

	String location = "MAIN PROGRAM";
    String parsedFileName;
	
	
	public F77BLOCElse() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
%}

%eofval{
    return getCheckResults();
%eofval}

/* A list of String is made, with identifiers such as DO and IF. Its aim  	*/
/* is to determine when an END is met if it corresponds to an IF.		  	*/
/* A list of boolean called elseFound determines whether and else statement */
/* is met.																	*/
/* An integer called LAST_STATE is used to switch to the last state seen	*/
/* when a line's continuation is found.										*/
/* A method called checkIdentifier is used to remove elseFound when an END	*/
/* corresponding to an IF is found.											*/
/* A setter called setElse is used to set value of an elseFound element.	*/
%{
	List<String> identifiers = new LinkedList<String>();
	List<Boolean> elseFound = new LinkedList<Boolean>();
	int LAST_STATE = LINE;
	
	private void checkIdentifier(final List<String> identifiers, final List<Boolean> elseFound) throws JFlexException {
		if (!identifiers.isEmpty()){
			int idLength = identifiers.size();
			if (identifiers.get(idLength-1).equals("IF")){
				int elseLength = elseFound.size();
				if (!elseFound.isEmpty() && !elseFound.get(elseLength-1)) {
					LOGGER.fine("Setting error line "+(yyline+1)+" because the IF instruction shall finish with an ELSE after the last ELSE IF.");
					this.setError(location,"The IF instruction shall finish with an ELSE after the last ELSE IF.", yyline + 1);
				}
				elseFound.remove(elseLength-1);
			}
			identifiers.remove(idLength-1);
		}
		yybegin(COMMENT);
	}
	
	private void setElse(List<Boolean> elseFound, boolean value) {
		if (!elseFound.isEmpty()) {
			elseFound.set(elseFound.size()-1, value);
		}
	}
%}

/* Transition word is else (or ELSE). We also look for DO, END, THEN and IF. */
/* FILE is set to avoid END FILE lines.										 */
RULE_WORD = else    | ELSE
DO		  = do		| DO
END       = end     | END
IF		  = if	    | IF
THEN      = then	| THEN
FILE	  = file    | FILE
CONT	  = CONTINUE| continue

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
<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
								    yybegin(COMMENT);
							    }
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/********************/
/* ELSE_LINE STATE	*/
/********************/
/* If an IF is found after an ELSE, we do not consider the previous ELSE. */
<ELSE_LINE>		{IF}			{setElse(elseFound, false);}
<ELSE_LINE>		.				{}
<ELSE_LINE>		\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ELSE_LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}

/****************/
/* IFLINE STATE	*/
/****************/
/* If THEN is encoutered, "IF" is stored in identifiers, else nothing's done. */
<IFLINE>		{THEN}			{identifiers.add("IF");
								 elseFound.add(true);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IFLINE -> LINE (Transition : THEN \""+yytext()+"\" )");
								 yybegin(LINE);}
<IFLINE>		.				{}	
<IFLINE>		\n				{LAST_STATE = IFLINE;
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IFLINE -> LINE (Transition : \\n )");
								 yybegin(NEW_LINE);}

/********************/
/*  ENDLINE STATE	*/
/********************/
<ENDLINE>		{FILE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENDLINE -> COMMENT (Transition : FILE \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<ENDLINE>		.				{checkIdentifier(identifiers, elseFound);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENDLINE -> COMMENT (Transition : . )");
								 yybegin(COMMENT);}
<ENDLINE>		\n				{checkIdentifier(identifiers, elseFound);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENDLINE -> COMMENT (Transition : \\n )");
								 yybegin(NEW_LINE);}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);}
<YYINITIAL>  	{TYPE}         	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<YYINITIAL>		{DO}			{identifiers.add("DO");
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : DO \""+yytext()+"\" )");
								    yybegin(LINE);}
<YYINITIAL>		{IF}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IFLINE (Transition : IF \""+yytext()+"\" )");
                                    yybegin(IFLINE);}
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENDLINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENDLINE -> LINE (Transition : . )");
                                    yybegin(LINE);}

/*********************/
/*	NEW LINE STATE	 */
/*********************/	
/* If ELSE is found, elseFound is set to true, if IF is found, elseFound is set	*/
/* to false. In that way, to avoid an error, an else must be the last met.		*/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
    								yybegin(NAMING);}
<NEW_LINE>		{DO}			{identifiers.add("DO");}	
<NEW_LINE>		{END} | {CONT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> ENDLINE (Transition : END or CONT \""+yytext()+"\" )");
                                    yybegin(ENDLINE);}
<NEW_LINE>		{RULE_WORD}		{setElse(elseFound, true);
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> ELSE_LINE (Transition : RULE_WORD \""+yytext()+"\" )");
								    yybegin(ELSE_LINE);}
<NEW_LINE>		{IF}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IFLINE (Transition : IF \""+yytext()+"\" )");
                                    yybegin(IFLINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext();
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<LINE>			{DO}			{identifiers.add("DO");}	
<LINE>			{END} | {CONT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> ENDLINE (Transition : END or CONT \""+yytext()+"\" )");
                                    yybegin(ENDLINE);}
<LINE>			{RULE_WORD}		{setElse(elseFound, true);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> ELSE_LINE (Transition : RULE_WORD \""+yytext()+"\" )");
								 yybegin(ELSE_LINE);}
<LINE>			{IF}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> IFLINE (Transition : IF \""+yytext()+"\" )");
                                    yybegin(IFLINE);}
<LINE>			{INT}			{}
<LINE>      	\n             	{LAST_STATE = LINE;
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
								    yybegin(NEW_LINE);}
<LINE>      	[^ \t]         	{
                                    if (yycolumn == 5){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> LAST_STATE (Transition : [^ \\t])");
                                        yybegin(LAST_STATE);
                                    }
                                }
<LINE>			.				{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                               }