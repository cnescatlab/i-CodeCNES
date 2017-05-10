/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Intent rule.	 */
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

%class F90INSTIntent
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, DECLARATION, PARAMS, DECL_PARAMS, AVOID_PARAMS

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
INTENT		 = INTENT 	  | intent
DATA_TYPE	 = INTEGER	  | integer	   | REAL 		| real 		|
			   LOGICAL	  | logical	   | CHARACER	| character | 
			   COMPLEX 	  | complex	   | DOUBLE[\ ]PRECISION 	| double[\ ]precision 
POINTER		 = POINTER	  | pointer
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
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
	List<String> params = new LinkedList<String>();
	boolean end = true;
	boolean intent = false;
	int par = 0;
	
	public F90INSTIntent() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
%}

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
<NAMING>		{VAR}			{params.clear(); par=0; location = location + " " + yytext(); 
								 if(location.toLowerCase().contains("subroutine")) yybegin(PARAMS);
								 else yybegin(COMMENT);}
<NAMING>    	\n             	{params.clear(); yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	{STRING}		{}
<DECL_PARAMS>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<DECL_PARAMS>	{INTENT}		{end=true; intent=true;}
<DECL_PARAMS>	{POINTER}		{end=true; yybegin(COMMENT);}
<DECL_PARAMS>	{SPACE}			{}
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>	\&				{end=false;}
<DECL_PARAMS>	\n				{if(end) intent=false; yybegin(NEW_LINE);}
<DECL_PARAMS>	.				{end=true;}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}			{end=true; if(!intent && params.contains(yytext())) setError(location,"It misses the attribute INTENT for the parameter " + yytext(), yyline+1);}
<DECLARATION>	\=				{yybegin(AVOID_PARAMS);}
<DECLARATION>	&				{end=false;}
<DECLARATION>	\n				{if(end) {intent=false; yybegin(NEW_LINE);}}
<DECLARATION>	.				{}


/************************/
/* PARAMS STATE    		*/
/************************/
<PARAMS>		{STRING}		{}
<PARAMS>		{VAR}			{params.add(yytext()); end=true;}
<PARAMS>		\(				{par++;}
<PARAMS>		\)				{par--; if(par==0) yybegin(COMMENT);}
<PARAMS>		&				{end=false;}
<PARAMS>		\n				{if(end) yybegin(NEW_LINE);}
<PARAMS>		.				{}


/************************/
/* AVOID_PARAMS STATE   */
/************************/
<AVOID_PARAMS>		{STRING}		{}
<AVOID_PARAMS>		\,				{yybegin(DECLARATION);}
<AVOID_PARAMS>		&				{end=false;}
<AVOID_PARAMS>		\n				{if(end) yybegin(NEW_LINE);}
<AVOID_PARAMS>		.				{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
