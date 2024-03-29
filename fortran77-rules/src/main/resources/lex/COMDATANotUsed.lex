/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.NotUsed rule. 		*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMDATANotUsed
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, DECL_PARAMS,  DECLARATION, DECL_VAR

COMMENT_WORD = \! 		  | C		  	| c		| \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
REAL_FUNC	 = "REAL" [\ ]* \(
IMPLICIT	 = "IMPLICIT"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
EQUAL		 = \= [^\,\n\"\']*
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMDATANotUsed.class.getName());

	String location = "MAIN PROGRAM";
	
	List<String> variables = new LinkedList<String>();
	List<String> locations = new LinkedList<String>();
	List<Integer> errors   = new LinkedList<Integer>();
	int par = 0;
    String parsedFileName;
	
	
	public COMDATANotUsed(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
        LOGGER.finest("end method setInputFile");
	}
	
	private void checkVar(String word) {
        LOGGER.finest("begin method checkVar");
		int index = variables.lastIndexOf(word.toLowerCase());
		if (index != -1) {
			errors.remove(index);
			locations.remove(index);
			variables.remove(index);
		} 
        LOGGER.finest("end method checkVar");
	}
	
	private void printError() throws JFlexException {
        LOGGER.finest("begin method printError");
		for (int i = 0; i < locations.size(); i++) {
            LOGGER.fine("Setting error line "+errors.get(i)+" for the variable "+ variables.get(i) +".");
			setError(locations.get(i),"The variable " + variables.get(i) + " is declared and not used.", errors.get(i));
		}
        LOGGER.finest("end method printError");
	}

	
%}

%eofval{
	printError();
	
return getCheckResults();
%eofval}
%eofclose

%%          

/************************/

				{FREE_COMMENT}	{
                				    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT )");
                				    yybegin(COMMENT);
                				}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{IMPLICIT}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : IMPLICIT \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{REAL_FUNC}		{}
<NEW_LINE>		{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECL_PARAMS (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECL_PARAMS);
                                }
<NEW_LINE>		{VAR}			{checkVar(yytext());}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{
                                    location=yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>			{STRING}       	{}
<LINE>			{IMPLICIT}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : IMPLICIT \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{REAL_FUNC}		{}
<LINE>			{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DECLARATION (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<LINE>			{VAR}			{checkVar(yytext());}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{TYPE}        	{
                                    location=yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<DECLARATION>	\(				{
                                    par=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> DECL_VAR (Transition : [(] )");
                                    yybegin(DECL_VAR);
                                }
<DECLARATION>	{VAR}			{variables.add(yytext().toLowerCase());
								 locations.add(location);
								 errors.add(yyline+1);}
<DECLARATION>	{EQUAL}			{}
<DECLARATION>  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<DECLARATION>  	.              	{}


/************************/
/* DECL_VAR STATE    	*/
/************************/
<DECL_VAR>		{VAR}        	{checkVar(yytext());}
<DECL_VAR>		\(				{par++;}
<DECL_VAR>		\)				{
                                    par--;
                                    if(par==0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECL_VAR -> DECLARATION (Transition : [)] )");
                                        yybegin(DECLARATION);
                                    }
                                }
<DECL_VAR>		[^] 			{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }