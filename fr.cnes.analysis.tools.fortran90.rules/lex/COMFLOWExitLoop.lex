/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Tr.BoucleSortie rule. 	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class COMFLOWExitLoop
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state called IFLINE is to determine whether the statement is stored. If */
/* it is followed by a THEN, we store it, else it's ignored.				 */
/* AVOID state is like COMMENT state but takes line continuation in count.	 */
%state COMMENT, NAMING, IFLINE, AVOID

COMMENT_WORD = "!" | "c" | "*"
FREE_COMMENT = "!"
TYPE		 = "function" | "procedure" | "subroutine" | "program" | "module"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"


%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	/** List of string used to store if and do statements. **/
	List<String> identifiers = new LinkedList<String>();
	/** Integer to determine imbrication depth. **/
	int depth = 0;
	/** A boolean to found continuation line. **/
	boolean ampFound = false;
	
	public COMFLOWExitLoop() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
%}

%eofval{
	return getCheckResults();
%eofval}

/* Transition word are STOP, EXIT, CYCLE and GO TO. If these words are met inside */
/* a DO-loop, an error is thrown.												  */
/* The word THEN is also a part of new words, as it helps to determine if an IF	  */
/* declaration expects an END word or not. This also goes with ELSE.			  */
/* Other word that can have an END statement are also added.					  */
STOP	  = [^a-zA-Z0-9\_]("stop")[^a-zA-Z0-9\_]
EXIT      = [^a-zA-Z0-9\_]("exit")[^a-zA-Z0-9\_]
CYCLE     = [^a-zA-Z0-9\_]("cycle")[^a-zA-Z0-9\_]
GOTO      = [^a-zA-Z0-9\_]("go"[\ \t\f]*"to")[^a-zA-Z0-9\_]
RULE_WORD = {STOP} | {EXIT} | {CYCLE} | {GOTO}
DO        = [^a-zA-Z0-9\_]("do")[^a-zA-Z0-9\_]
IF		  = [^a-zA-Z0-9\_]("if")[^a-zA-Z0-9\_]
ELSE	  = [^a-zA-Z0-9\_]("else")[^a-zA-Z0-9\_]
THEN	  = [^a-zA-Z0-9\_]("then")[^a-zA-Z0-9\_]
SELECT	  = [^a-zA-Z0-9\_]("select")[^a-zA-Z0-9\_]
END       = [^a-zA-Z0-9\_]("end")[^a-zA-Z0-9\_]{SPACE}*({IF} | {SELECT} | {DO} | {TYPE})?

%%          
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/
				{FREE_COMMENT}	{yybegin(COMMENT);}

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	
		{
			(\n|\r)+        {yybegin(YYINITIAL);}  
			.              	{}
		}
		
/*********************/
/*	AVOID STATE		 */
/*********************/
<AVOID>   	
		{
			"&"{SPACE}*[^\n\r]	{}
			"&"					{ampFound = true;}
			[^]            		{
								if (!ampFound){
									yybegin(YYINITIAL);
								 }
								 ampFound = false;
								}
	
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext();
							 yybegin(COMMENT);}
			(\n|\r)+        {yybegin(YYINITIAL);}
			.              	{}
		}

/*********************/
/*	IF LINE PART	 */
/*********************/
/* If THEN is encoutered, "IF" is stored in identifiers, else nothing's done. */
<IFLINE>		
		{
			"&"{SPACE}*[^\n\r]	{}
			"&"					{ampFound = true;}
			{THEN}				{identifiers.add("IF");
								 yybegin(AVOID);}
			{RULE_WORD}			{this.setError(location,"There is more than one exit in the loop.", yyline + 1);}
			.					{}
			(\n|\r)+			{if (!ampFound){
									yybegin(YYINITIAL);
								 }
								 ampFound = false;
								}
		}

/*********************/
/*	INITIAL STATE	 */
/*********************/	
/* If END is found, we check whether it corresponds to a DO-loop. If it is	*/
/* we decrease depth value by 1. Then, we remove last identifier.			*/
<YYINITIAL> 
		{
		    {COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							yybegin(NAMING);}
			{DO}			{identifiers.add("DO");
							 depth++;}
			{END}			{if (!identifiers.isEmpty()) {
								int idLength = identifiers.size();
								if (identifiers.get(idLength-1).equals("DO")) {
									depth--;
								}
								identifiers.remove(idLength-1);
							 }
							 yybegin(COMMENT);
							}
			{ELSE}			{}
			{IF}			{yybegin(IFLINE);}
			
			{RULE_WORD}		{if (depth > 0) {
							 	this.setError(location,"There is more than one exit in the loop.", yyline + 1);
							 }
							}
			[^]             {}
		}
			
/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }