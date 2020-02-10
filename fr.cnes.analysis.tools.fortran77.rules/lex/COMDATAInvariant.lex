/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Invariant rule. 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInvariant
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, DECL_PARAMS, DECLARATION, WAIT_DECL, FUNC_DEC, FUNCTION

COMMENT_WORD = \!         | c          | C     | \*
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
CALL		 = "CALL"
FUNC_CALL	 = \= [\ ]* {VAR}
VAR_MODIF	 = {VAR} (\([^\)]*\))?[\ ]*\=
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	
	private static final Logger LOGGER = Logger.getLogger(COMDATAInvariant.class.getName());
	
	String location = "MAIN PROGRAM";
	
	List<String> variables = new LinkedList<String>();
	//mantis 322 use to keep used variables
	List<String> usedVariables = new LinkedList<String>();
	List<String> locations = new LinkedList<String>();
	List<Integer> errors   = new LinkedList<Integer>();
	
	String parsedFileName;
	
	public COMDATAInvariant(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	private void checkVar(String word) {
        LOGGER.finest("begin method checkVar");
		if (word.contains("(")) word = word.split("\\(")[0];
		int index = variables.lastIndexOf(word);
		if (index != -1) {
			variables.remove(index);
			locations.remove(index);
			errors.remove(index);
		}
        LOGGER.finest("end method checkVar");
		yybegin(FUNCTION);
	}
	
	private void printError() throws JFlexException {
        LOGGER.finest("begin method printError");
		for(int i=0; i < locations.size(); i++) {
			if(usedVariables.contains(variables.get(i))){
		        if(errors.size() > i){
		        	LOGGER.fine("Setting error line "+errors.get(i)+" for the variable "+variables.get(i)+".");
					setError(locations.get(i),"The variable " + variables.get(i) + " must be defined as constant.", errors.get(i));
 				}else{
                    		
				            final String errorMessage = "Error's line of variable's "+ variables.get(i)+" violation unreachable.";
				            throw new JFlexException(this.getClass().getName(), parsedFileName,
				                            errorMessage, yytext(), yyline, yycolumn);
 				}
            }		
		}
        LOGGER.finest("end method printError");
	}

	
%}

%eofval{
	printError();
	
return getCheckResults();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{
                				    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\")");
                				    yybegin(COMMENT);
                				}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\")");
                                    yybegin(COMMENT);
                                }
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n)");
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
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    location = yytext();
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{CALL}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> FUNC_DEC (Transition : CALL \""+yytext()+"\" )");
                                    yybegin(FUNC_DEC);
                                }
<NEW_LINE>		{FUNC_CALL}		{
                                    String var = yytext().substring(1).trim();
                                    usedVariables.add(var);		
                                    if(!variables.contains(var)){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> FUNC_DEC (Transition : FUNC_CALL \""+yytext()+"\" )");
                                        yybegin(FUNC_DEC);
                                    }
								 }
<NEW_LINE>		{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECLARATION (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<NEW_LINE>		{VAR_MODIF}		{
                                    String var = yytext().substring(0, yytext().length()-1).trim();
                                    usedVariables.add(var);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> FUNCTION (Transition : VAR_MODIF ) by running checkVar("+var+")");
                                    checkVar(var);
                                }
<NEW_LINE>      {VAR}			{String var = yytext().trim();usedVariables.add(var);}
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
<LINE>			{STRING}		{}
<LINE>			{CALL}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> FUNC_DEC (Transition : CALL \""+yytext()+"\" )");
                                    yybegin(FUNC_DEC);
                                }
<LINE>			{FUNC_CALL}		{
                                    String var = yytext().substring(1).trim();
                                    if(!variables.contains(var)){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> FUNC_DEC (Transition : FUNC_CALL \""+yytext()+"\" )");
                                        yybegin(FUNC_DEC);
                                    }
                                }
<LINE>			{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DECLARATION (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<LINE>			{VAR_MODIF}		{
                                    String var = yytext().substring(0, yytext().length()-1).trim(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> FUNCTION (Transition : VAR_MODIF ) by running checkVar("+var+")");
                                    checkVar(var);}
<LINE>          {VAR}			{String var = yytext().trim();usedVariables.add(var);}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{TYPE}        	{
                                    location=yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}			{variables.add(yytext());
								 locations.add(location);
								 errors.add(yyline+1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> WAIT_DECL (Transition : VAR \""+yytext()+"\" )");
								 yybegin(WAIT_DECL);}
<DECLARATION>  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<DECLARATION>  	.              	{}


/************************/
/* WAIT_DECL STATE      */
/************************/
<WAIT_DECL>		{STRING}		{}
<WAIT_DECL>		\,				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT_DECL -> DECLARATION (Transition : [,] )");
                                    yybegin(DECLARATION);
                                }
<WAIT_DECL>  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT_DECL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<WAIT_DECL>  	.              	{}


/************************/
/* FUNC_DEC STATE       */
/************************/
<FUNC_DEC>		\(				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNC_DEC -> FUNCTION (Transition : [(] )");
                                    yybegin(FUNCTION);
                                }
<FUNC_DEC>  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNC_DEC -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<FUNC_DEC>  	.              	{}


/************************/
/* FUNCTION STATE       */
/************************/
<FUNCTION>		{VAR}			{String var = yytext().trim();usedVariables.add(var);checkVar(var);}
<FUNCTION>  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNCTION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<FUNCTION>  	.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
				                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }