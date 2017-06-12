/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.Array rule.		 */
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

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90REFArray
%extends AbstractChecker
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, ARRAY_CHECK, ARRAY_DEC, ARRAY_PAR, ARRAY_DEC_NAME, ARRAY_WAIT, AVOID, BRACKET

COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
DIMENSION	 = DIMENSION  | dimension
ARRAY		 = {DIMENSION} [\ ]* \( {VAR} | {INT} \) [\ ]+ \:\:
OPEN_PAR	 = [\ ]* \(
ARRAY_NOTAT  = (\-)? ( ({INT}|{VAR}) ( {OPER} ({INT}|{VAR}) )* | \: ) [^\)]* \) 
DATA		 = "data"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+
OPER		 = \*		  | \*\*	   | \+		  | \-		   | "/"&&!"//"
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f]

%{
	String location = "MAIN PROGRAM"; 
	/** List where to save the name of all the arrays **/
	List<String> arrays = new LinkedList<String>();
	/** Variable to print with the error **/
	String variable = "";
	String variableFinal = "";
	/** Determine if the line is an expression **/
	boolean expression = false;
	/** Boolean to verify the end of line **/
	boolean end = true;
	/** Boolean to determine error **/
	boolean error = false;
	/** Integer t count the number of pair of brackets **/
	int par = 0;
	
	
	public F90REFArray() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
%}

%eofval{ 
 return getCheckResults(); 
%eofval}


%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/************************/
/* AVOID STATE        */
/************************/
<AVOID>		{SPACE}			{}
<AVOID>		\&				{end = false;}
<AVOID>   	\n             	{if(end) yybegin(NEW_LINE);}
<AVOID>   	.              	{end = true;}

/************************/
/* NAMING STATE         */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); arrays.clear(); yybegin(COMMENT);}
<NAMING>    	\n             	{expression=false; variableFinal = ""; yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{ARRAY}			{yybegin(ARRAY_DEC);}
<YYINITIAL>		{DATA}			{yybegin(COMMENT);}
<YYINITIAL> 	\n             	{expression=false; yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DIMENSION}		{yybegin(ARRAY_DEC);}
<NEW_LINE>		{OPER}			{expression=true;}
<NEW_LINE>		{DATA}			{yybegin(AVOID);}
<NEW_LINE>		{VAR}			{if(arrays.contains(yytext().toLowerCase())) { variable = yytext().toLowerCase(); yybegin(ARRAY_PAR); } }
<NEW_LINE>  	\n             	{expression=false; variableFinal = "";}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{DIMENSION}		{end = true; yybegin(ARRAY_DEC);}
<LINE>			{OPER}			{end = true; expression = true;}
<LINE>			{DATA}			{yybegin(AVOID);}
<LINE>			{VAR}			{end = true;  if(arrays.contains(yytext().toLowerCase())) { variable = yytext().toLowerCase(); yybegin(ARRAY_PAR); } }
<LINE>			\(				{par=1; yybegin(BRACKET);}
<LINE>			\&				{end = false;}
<LINE>			\,				{expression = false;}
<LINE>      	\n             	{if (end) { expression=false; variableFinal = ""; yybegin(NEW_LINE); } }
<LINE>      	.              	{}


/************************/
/* ARRAY_DEC STATE      */
/************************/
<ARRAY_DEC>	  	\:\:         	{yybegin(ARRAY_DEC_NAME);}
<ARRAY_DEC>   	\n             	{expression=false; variableFinal = ""; yybegin(NEW_LINE);}
<ARRAY_DEC>    	.              	{}


/*****************************/
/* ARRAY_DEC_NAME STATE      */
/*****************************/
<ARRAY_DEC_NAME>	{VAR}		{arrays.add(yytext().toLowerCase());}
<ARRAY_DEC_NAME>	\=			{par=0; yybegin(ARRAY_WAIT);}
<ARRAY_DEC_NAME>   	\n         	{expression=false; variableFinal = ""; yybegin(NEW_LINE);}
<ARRAY_DEC_NAME>   	.          	{}

/*****************************/
/* ARRAY_WAIT STATE      */
/*****************************/
<ARRAY_WAIT>	\(			{par++;}
<ARRAY_WAIT>	\)			{par--;}
<ARRAY_WAIT>	\,			{if(par==0) yybegin(ARRAY_DEC_NAME);}
<ARRAY_WAIT>	\&			{end = false;}
<ARRAY_WAIT>   	\n         	{if (end) { expression=false; variableFinal = ""; yybegin(NEW_LINE);} }
<ARRAY_WAIT>   	.          	{end = true;}

/*****************************/
/* BRACKET STATE      */
/*****************************/
<BRACKET>		\(			{par++;}
<BRACKET>		\)			{par--; if(par==0) yybegin(LINE);}
<BRACKET>   	[^]    	{}


/************************/
/* ARRAY_PAR STATE      */
/************************/
<ARRAY_PAR>		{OPEN_PAR}  	{yybegin(ARRAY_CHECK);}
<ARRAY_PAR>		{OPER}			{expression=true; variableFinal = variableFinal + " " + variable; yybegin(LINE);}
<ARRAY_PAR>		\=				{variableFinal = variableFinal + " " + variable; yybegin(LINE);}
<ARRAY_PAR>		{SPACE}			{}
<ARRAY_PAR>		\&				{end = false; }
<ARRAY_PAR>  	\n             	{if (end) {
									if(expression) setError(location,"It should be used the notation(:) to specify the entire use of the arrays:  " + variableFinal + " " + variable, yyline+1);
									expression = false;
									variableFinal = "";
									yybegin(NEW_LINE);
								 } 
								}
<ARRAY_PAR>  	.              	{}


/************************/
/* ARRAY_CHECK STATE    */
/************************/
<ARRAY_CHECK>	{ARRAY_NOTAT}  	{yybegin(LINE);}
<ARRAY_CHECK>	{OPER}			{expression=true; variableFinal = variableFinal + " " + variable; yybegin(LINE);}
<ARRAY_CHECK>	\=				{variableFinal = variableFinal + " " + variable; yybegin(LINE);}
<ARRAY_CHECK>	{SPACE}			{}
<ARRAY_CHECK>	\&				{end = false; }
<ARRAY_CHECK>  	\n             	{if (end) {
									if(expression) setError(location,"It should be used the notation(:) to specify the entire use of the arrays:  " + variableFinal + " " + variable, yyline+1);
									expression = false;
									variableFinal = "";
									yybegin(NEW_LINE);
								 } 
								}
<ARRAY_CHECK>  	.              	{}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
