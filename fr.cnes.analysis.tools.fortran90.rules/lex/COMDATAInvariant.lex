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

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInvariant
%extends AbstractChecker
%public
%column
%line
%ignorecase

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
PARAM_IN     = "INTENT" [\ ]* \( [\ ]* "IN" [\ ]* \)
CALL		 = "CALL"
FUNC_CALL	 = \= [\ ]* {VAR}
VAR_MODIF	 = {VAR} (\([^\)]*\))?[\ ]*\=
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> variables = new LinkedList<String>();
		//mantis 322 use to keep used variables
	List<String> usedVariables = new LinkedList<String>();
	List<String> locations = new LinkedList<String>();
	List<Integer> errors   = new LinkedList<Integer>();
	boolean end = true;
	
	public COMDATAInvariant(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void checkVar(String word) {
		// word is like "a ="
		//word = word.substring(0, word.length()-1).trim();
		if (word.contains("(")) word = word.split("\\(")[0];
		int index = variables.lastIndexOf(word);
		if (index != -1) {
			variables.remove(index);
			locations.remove(index);
			errors.remove(index);
		}
		yybegin(FUNCTION);
	}
	
	private void printError() throws JFlexException {
		for(int i=0; i < locations.size(); i++) {
			setError(locations.get(i),"The variable " + variables.get(i) + " must be defined as constant.", errors.get(i));
		}
	}

	
%}

%eofval{
	printError();
	
	return getCheckResults();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{CALL}			{yybegin(FUNC_DEC);}
<NEW_LINE>		{FUNC_CALL}		{String var = yytext().substring(1).trim();usedVariables.add(var); if(!variables.contains(var)) yybegin(FUNC_DEC);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<NEW_LINE>		{VAR_MODIF}		{String var = yytext().substring(0, yytext().length()-1).trim(); usedVariables.add(var); checkVar(var);}
<NEW_LINE>      {VAR}			{String var = yytext().trim();usedVariables.add(var);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{location=yytext(); yybegin(NAMING);}
<LINE>			{STRING}		{}
<LINE>			{CALL}			{yybegin(FUNC_DEC);}
<LINE>			{FUNC_CALL}		{String var = yytext().substring(1).trim();usedVariables.add(var); if(!variables.contains(var)) yybegin(FUNC_DEC);}
<LINE>			{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<LINE>			{VAR_MODIF}		{String var = yytext().substring(0, yytext().length()-1).trim();usedVariables.add(var); checkVar(var);}
<LINE>          {VAR}			{String var = yytext().trim();usedVariables.add(var);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	{TYPE}        	{location=yytext(); yybegin(NAMING);}
<DECL_PARAMS>	{STRING}		{}
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>	{PARAM_IN}		{yybegin(COMMENT);}
<DECL_PARAMS>  	\n             	{yybegin(NEW_LINE);}
<DECL_PARAMS>  	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}			{variables.add(yytext());
								 locations.add(location);
								 errors.add(yyline+1);
								 yybegin(WAIT_DECL);}
<DECLARATION>  	\n             	{yybegin(NEW_LINE);}
<DECLARATION>  	.              	{}


/************************/
/* WAIT_DECL STATE      */
/************************/
<WAIT_DECL>		{STRING}		{}
<WAIT_DECL>		\,				{yybegin(DECLARATION);}
<WAIT_DECL>  	\n             	{yybegin(NEW_LINE);}
<WAIT_DECL>  	.              	{}

/************************/
/* FUNC_DEC STATE       */
/************************/
<FUNC_DEC>		\(				{yybegin(FUNCTION);}
<FUNC_DEC>  	\n             	{yybegin(NEW_LINE);}
<FUNC_DEC>  	.              	{}


/************************/
/* FUNCTION STATE       */
/************************/
<FUNCTION>		{VAR}			{end=true;String var = yytext().trim();usedVariables.add(var); checkVar(var);}
<FUNCTION>		&				{end=false;}
<FUNCTION>  	\n             	{if(end) yybegin(NEW_LINE);}
<FUNCTION>  	.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }