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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class COMFLOWExitLoop
%extends AbstractRule
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<Violation>

/* A state called IFLINE is to determine whether the statement is stored. If */
/* it is followed by a THEN, we store it, else it's ignored.				 */
/* AVOID state is like COMMENT state but takes line continuation in count.	 */
%state COMMENT, NAMING, NEW_LINE, IFLINE, AVOID

COMMENT_WORD = "!" | "c" | "*"
FREE_COMMENT = "!"
TYPE		 = "function" | "procedure" | "subroutine" | "program" | "module"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"


%{
    private static final Logger LOGGER = Logger.getLogger(COMFLOWExitLoop.class.getName());


	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	
	/** List of string used to store if and do statements. **/
	List<String> identifiers = new LinkedList<String>();
	/** Integer to determine imbrication depth. **/
	int depth = 0;
	/** A boolean to found continuation line. **/
	boolean ampFound = false;
    /** Name of the file currently parsed */
    String parsedFileName;
    
    	
	public COMFLOWExitLoop() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(file.toOSString());
        LOGGER.finest("end method setInputFile");
	}
%}

%eofval{
return getViolations();
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
                {FREE_COMMENT}  {
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	
		{
			(\n|\r)+        {
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n|\\r )");
			                     yybegin(YYINITIAL);
		                    }  
			.              	{}
		}
		
/*********************/
/*	AVOID STATE		 */
/*********************/
<AVOID>   	
		{
			"&"{SPACE}*[^\n\r]	{ampFound = true;}
			"&"					{ampFound = true;}
			[^]            		{if (!ampFound){
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> YYINITIAL (Transition : \\n|\\r )");
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
							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
							 yybegin(COMMENT);}
			(\n|\r)        {
			                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n|\\r )");
			                 yybegin(YYINITIAL);}
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
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IFLINE -> AVOID (Transition : THEN \""+yytext()+"\" )");
								 yybegin(AVOID);}
			{RULE_WORD}			{
			                     LOGGER.fine("Setting error line "+(yyline+1)+" cause there is more than one exit in the loop.");
			                     this.setError(location,"There is more than one exit in the loop.", yyline + 1);}
			.					{}
			(\n|\r)			{if (!ampFound){
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IFLINE -> YYINITIAL (Transition : \\n|\\r )");
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
		    {COMMENT_WORD} 	{
		                      LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
		                      yybegin(COMMENT);}
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext();
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
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
							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : END \""+yytext()+"\" )");
							 yybegin(COMMENT);
							}
			{ELSE}			{}
			{IF}			{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IFLINE (Transition : IF \""+yytext()+"\" )");
			                     yybegin(IFLINE);}
			{RULE_WORD}		{if (depth > 0) {
							 	LOGGER.fine("Setting error line "+(yyline+1)+" cause there is more than one exit in the loop.");
							 	this.setError(location,"There is more than one exit in the loop.", yyline + 1);
							 }
							}
			[^]        		{}
		}
		

/*********************/
/*	ERROR THROWN	 */
/*********************/
			[^]			{
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+(yyline+1)+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                        }