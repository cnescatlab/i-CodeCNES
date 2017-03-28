/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*********************************************************************************/
/* This file is used to generate a rule checker for F90.DATA.ConstantFloat rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 	 */
/* As many comments have been done on the ExampleRule.lex file, this file    	 */
/* will restrain its comments on modifications.								 	 */
/*																			 	 */
/*********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DATAConstantFloat
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, CONSTANT, CONST_DEF

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
REAL		 = REAL		  | real
CONSTANT	 = \( [\ ]* {VAR} [\ ]* \)
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
NUM			 = [0-9]* \. [0-9]+   ( ("D" | "E")(\+ | \-)[0-9]+ )?   | 
			   [0-9]+ \. [0-9]*   ( ("D" | "E")(\+ | \-)[0-9]+ )?
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
%{
	String location = "MAIN PROGRAM"; 
 	 List<Violation> list = new LinkedList<Violation>();
	String constant = null;
	String variable = null;
	boolean wellDef = false;
	boolean cte= false;
	boolean decl = false;
	
	
	public F90DATAConstantFloat() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	
	
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
  
 return getViolations(); 
%eofval}


%%          


				{COMMENT_WORD}	{yybegin(COMMENT);}
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{TYPE}         	{cte=false; location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{REAL}			{cte=false; yybegin(CONSTANT);}
<NEW_LINE>  	\n             	{cte=false; }
<NEW_LINE>  	.              	{cte=false; yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{REAL}			{yybegin(CONSTANT);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* CONSTANT STATE       */
/************************/
<CONSTANT>	  	{CONSTANT}		{constant = yytext().substring(1, yytext().length() -1).toLowerCase().trim(); cte=true;}
<CONSTANT>		\:\:			{decl=true;}
<CONSTANT>		{NUM}			{if(cte && decl) yybegin(CONST_DEF);}
<CONSTANT>		{VAR}			{if(decl)variable=yytext();}
<CONSTANT>		\n				{cte=false; decl=false;yybegin(NEW_LINE);}
<CONSTANT>		.				{}


/************************/
/* CONST_DEF STATE       */
/************************/
<CONST_DEF>	  	\_{VAR}			{String word = yytext().substring(1).toLowerCase();
								 if(word.equals(constant)) wellDef = true;}
<CONST_DEF>		\n				{if(!wellDef) setError(location,"Float constant " + variable + " shall be declared using the subtype_parameter: <name>_<subtype_parameter>.", yyline+1);
								 wellDef = false; cte = false;  decl = false; yybegin(NEW_LINE);}
<CONST_DEF>		.				{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
