/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DATA.ArrayAccess rule*/
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
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F90DATAArrayAccess
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
%state COMMENT, NAMING, NEW_LINE, LINE, ARRAY,INDIRECTION, ARRAY_PARAM, ARRAY_DEC

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
DIMENSION	 = "dimension"{SPACE}*\(
ARRAY_VAR	 = {VAR} [\ ]* \( 
VAR_INIT	 = {VAR} {SPACE}* \= {SPACE}* \( \/
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f]

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
 	/** Arrays declared in the file **/
 	List<String> arrays = new LinkedList<String>();
 	List<String> arraysInd = new LinkedList<String>();
 	/** Parameters to make checkings **/
	int par = 0;
	boolean end = true;
	String array = "";
	boolean indirectionRep = false;
	List<String> indirect = new LinkedList<String>();
	
	public F90DATAArrayAccess() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
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
<NAMING>		{VAR}			{location = location + " " + yytext(); arrays.clear(); yybegin(COMMENT);}
<NAMING>    	\n             	{arrays.clear(); yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DIMENSION}		{yybegin(ARRAY_PARAM);}
<NEW_LINE>		{ARRAY_VAR}		{array = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
								 if(arrays.contains(array)) {par=1; yybegin(ARRAY);}}
<NEW_LINE>		{VAR_INIT}		{array = yytext().replaceAll("\\(","").replaceAll("=","").replaceAll("/","").trim(); 
								 indirectionRep=false; yybegin(INDIRECTION);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<LINE>			{STRING}		{yybegin(LINE);}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{DIMENSION}		{yybegin(ARRAY_PARAM);}
<LINE>			{ARRAY_VAR}		{array = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
								 if(arrays.contains(array)) {par=1; yybegin(ARRAY);}}
<LINE>			{VAR_INIT}		{array = yytext().replaceAll("\\(","").replaceAll("=","").replaceAll("/","").trim(); 
								 indirectionRep=false; yybegin(INDIRECTION);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

/************************/
/* ARRAY_PARAM STATE    */
/************************/
<ARRAY_PARAM>	\:\:			{yybegin(ARRAY_DEC);}
<ARRAY_PARAM>	\&				{end=false;}
<ARRAY_PARAM>	\n				{if(end) yybegin(NEW_LINE);
								 end=true;}
<ARRAY_PARAM>	.				{}

/************************/
/* ARRAY_DEC STATE      */
/************************/
<ARRAY_DEC>		{VAR_INIT}		{array = yytext().replaceAll("\\(","").replaceAll("=","").replaceAll("/","").trim(); 
								 arrays.add(array); indirectionRep=false; yybegin(INDIRECTION);}
<ARRAY_DEC>		{VAR}			{arrays.add(yytext());}
<ARRAY_DEC>		\&				{end=false;}
<ARRAY_DEC>		\n				{if(end) yybegin(NEW_LINE);
								 end=true;}
<ARRAY_DEC>		.				{}

/************************/
/* INDIRECTION STATE    */
/************************/
<INDIRECTION>	  	{VAR} | {INT} 	{if(!indirectionRep) {
										String val = yytext();
									 	if(indirect.contains(val)) indirectionRep = true;
									 	else indirect.add(val);
									 }
									}
<INDIRECTION>		\/[\ ]*\)		{if(indirectionRep) arraysInd.add(array);
									 indirect.clear();
									 yybegin(LINE);}
<INDIRECTION>		[^] 			{}


/************************/
/* ARRAY STATE		    */
/************************/
<ARRAY>			\(				{par++;}
<ARRAY>			\)				{par--; if(par==0) yybegin(LINE);}
<ARRAY>			{ARRAY_VAR}		{String var = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
								 if(arrays.contains(var)) { array=var;par=1; }
 								 else yybegin(LINE); }
<ARRAY>			{VAR}[\ ]*\%	{}
<ARRAY>		  	{VAR}		 	{if(arraysInd.contains(yytext().toLowerCase())) setError(location,"Array " + array + 
																		" initialized using other array named "
																		+ yytext() + " with repeated values.", yyline+1);
 								 else yybegin(LINE);}
<ARRAY>			\n				{yybegin(NEW_LINE);}
<ARRAY>			.				{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
