/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77BLOCLoop rule.		 */
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

%class F77BLOCLoop
%extends AbstractRule
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<Violation>

/* A new state named DO_LINE is created in order to verify the DO rule */
%state COMMENT, NAMING, NEW_LINE, LINE, DO_LINE

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
    private static final Logger LOGGER = Logger.getLogger(F77BLOCLoop.class.getName());

	String location = "MAIN PROGRAM";
	String parsedFileName;	
	
	List<String> identifiers = new LinkedList<String>();
	
	public F77BLOCLoop() {
    }

	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(file.toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	private void checkIndentifiers(String id) throws JFlexException {
		LOGGER.finest("begin method checkIndentifiers");
		Boolean found = false;
		int size = identifiers.size();
		for (int i = 0; i < size; i++) {
			if (id.equals(identifiers.get(i)) && !found) {
				LOGGER.fine("Setting error line "+ (yyline+1) +" for the function "+ location+ ".");
				this.setError(location,"Loops shall have distinct ends.", yyline + 1);
				found = true;
			}
		}
		if (!found) {
			identifiers.add(id);
		}
		LOGGER.finest("end method checkIndentifiers");
	}
%}

%eofval{
return getViolations();
%eofval}

/* Transition word is else (or ELSE). We also look for DO, END, THEN and IF. */
DO		  = do		 | DO
END       = end      | END
INT		  = [0-9]+

%%          

				{FREE_COMMENT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");									
									yybegin(COMMENT);
									
								}

<COMMENT>   	\n             	{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);
								}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
									location = location + " " + yytext();
								 	identifiers.clear();
								 	yybegin(COMMENT);
							 	}
<NAMING>    	\n             	{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
									identifiers.clear();
									yybegin(NEW_LINE);
								}
<NAMING>    	.              	{}

/* If a DO is found, go to state DO_LINE to verify the rule */
<DO_LINE>		{INT}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_LINE -> COMMENT (Transition : INT \""+yytext()+"\" )");
									checkIndentifiers(yytext());
								 	yybegin(COMMENT);
							 	}
<DO_LINE>		{VAR}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_LINE -> LINE (Transition : VAR \""+yytext()+"\" )");									
									yybegin(LINE);}	
<DO_LINE>    	\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_LINE -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);}
<DO_LINE>		.				{}


<YYINITIAL>  	{COMMENT_WORD} 	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
									yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : STRING \""+yytext()+"\" )");
									yybegin(LINE);}
<YYINITIAL>	  	{TYPE}         	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
									location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{DO}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> DO_LINE (Transition : DO \""+yytext()+"\" )");
									yybegin(DO_LINE);}
<YYINITIAL> 	\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
									yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
									yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
									yybegin(LINE);}
<NEW_LINE>	  	{TYPE}         	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
									location = yytext(); 
									yybegin(NAMING);}
<NEW_LINE>		{DO}			{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DO_LINE (Transition : DO \""+yytext()+"\" )");
									yybegin(DO_LINE);
									
								}	
<NEW_LINE>		{INT}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : INT \""+yytext()+"\" )");
									yybegin(LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
									yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : COMMENT_WORD \""+yytext()+"\" )");
									location = yytext();
									yybegin(NAMING);}
<LINE>			{DO}			{	
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DO_LINE (Transition : DO \""+yytext()+"\" )");
									yybegin(DO_LINE);}	
<LINE>			{END}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> LINE (Transition : END \""+yytext()+"\" )");
									yybegin(LINE);}
<LINE>      	\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}