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

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATANotUsed
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, DECL_PARAMS, DECLARATION, DECL_VAR

COMMENT_WORD = \! 
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision  | TYPE	   | type 
REAL_FUNC	 = "REAL" [\ ]* \(
IMPLICIT	 = "IMPLICIT"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
EQUAL		 = \= [^\,\n\"\']*
																
%{
	String location = "MAIN PROGRAM";
	List<String> variables = new LinkedList<String>();
	List<String> locations = new LinkedList<String>();
	List<Integer> errors   = new LinkedList<Integer>();
	int par = 0;
	
	public COMDATANotUsed(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void checkVar(String word) {
		int index = variables.lastIndexOf(word.toLowerCase());
		if (index != -1) {
			errors.remove(index);
			locations.remove(index);
			variables.remove(index);
		} 
	}
	
	private void printError() throws JFlexException {
		for (int i = 0; i < locations.size(); i++) {
			setError(locations.get(i),"The variable " + variables.get(i) + " is declared and not used.", errors.get(i));
		}
	}

	
%}

%eofval{
	printError();
	
    
	
	return getViolations();
%eofval}


%%          

/************************/

				{COMMENT_WORD}	{yybegin(COMMENT);}

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
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{IMPLICIT}		{yybegin(COMMENT);}
<NEW_LINE>		{REAL_FUNC}		{}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<NEW_LINE>		{VAR}			{checkVar(yytext());}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{location=yytext(); yybegin(NAMING);}
<LINE>			{STRING}       	{}
<LINE>			{IMPLICIT}		{yybegin(COMMENT);}
<LINE>			{REAL_FUNC}		{}
<LINE>			{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<LINE>			{VAR}			{checkVar(yytext());}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>	{VAR}			{checkVar(yytext());}
<DECL_PARAMS>  	\n             	{yybegin(NEW_LINE);}
<DECL_PARAMS>  	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}			{variables.add(yytext().toLowerCase());
								 locations.add(location);
								 errors.add(yyline+1);}
<DECLARATION>	{EQUAL}			{}
<DECLARATION>	\(				{par=1; yybegin(DECL_VAR);}
<DECLARATION>  	\n             	{yybegin(NEW_LINE);}
<DECLARATION>  	.              	{}

/************************/
/* DECL_VAR STATE    	*/
/************************/
<DECL_VAR>		{VAR}        	{checkVar(yytext());}
<DECL_VAR>		\(				{par++;}
<DECL_VAR>		\)				{par--; if(par==0) yybegin(DECLARATION);}
<DECL_VAR>		\n				{yybegin(NEW_LINE);}
<DECL_VAR>		.				{}



/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}