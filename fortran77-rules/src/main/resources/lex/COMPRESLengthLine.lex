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

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import java.util.logging.Logger;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMPRESLengthLine
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
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMPRESLengthLine.class.getName());
	String location = "MAIN PROGRAM";
    String parsedFileName;
	
	int chars = 1;

	
	public COMPRESLengthLine(){
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

%%          

/************************/

				{FREE_COMMENT}	{
                                    chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
				                    yybegin(COMMENT);
                                }

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{
                                    if(chars>100){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because there are more than 100 characters in this line.");
                                        setError(location,"There are more than 100 characters in this line.", yyline+1);
                                    }
                                    chars=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }  
<COMMENT>   	.              	{chars+=yytext().length();}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{chars+=yytext().length(); location = location + " " + yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NAMING>    	\n             	{
                                    if(chars>100){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because there are more than 100 characters in this line.");
                                        setError(location,"There are more than 100 characters in this line.", yyline+1);
                                    }
                                    chars=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{chars+=yytext().length();}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>		{TYPE}        	{chars+=yytext().length(); location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<YYINITIAL> 	\n             	{
                                    if(chars>100){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because there are more than 100 characters in this line.");
                                        setError(location,"There are more than 100 characters in this line.", yyline+1);
                                    }
                                    chars=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{chars+=yytext().length(); location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<NEW_LINE>  	\n             	{
                                    if(chars>100){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because there are more than 100 characters in this line.");
                                        setError(location,"There are more than 100 characters in this line.", yyline+1);
                                    }
                                    chars=1;
                                }
<NEW_LINE>  	.              	{ 
                                    chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{
                                    chars+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>      	\n             	{
                                    if(chars>100){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because there are more than 100 characters in this line.");
                                        setError(location,"There are more than 100 characters in this line.", yyline+1);
                                    }
                                    chars=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{chars+=yytext().length();}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                               }