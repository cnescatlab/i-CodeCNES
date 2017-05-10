/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DATA.Array rule.	 */
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

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DATAArray
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
%state COMMENT, NAMING, NEW_LINE, LINE, PARAM, DECLARATION, WAIT, VERIFICATION, VERIFICATION_DIM, WAIT_2, VERIF_PARAM

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

COMMENT_WORD = \!
TYPE		 = "function" | "procedure"| "subroutine"	| "program" | "module" | "interface"
DATA_TYPE 	 = "integer"  | "real"	   | "complex" 		| "double"{SPACE}*"precision"	|
			   "logical"  | "character"		   
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f\r]


/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
%{
	String location = "MAIN PROGRAM"; 
	/** LIst of violations **/
  	List<Violation> list = new LinkedList<Violation>();
  	/** Parameters' function list **/
  	List<String> parameters = new LinkedList<String>();
  	int parenthese = 0;
  	boolean error = false;
	
	public F90DATAArray() {
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
<COMMENT> 
		{
		  	\n             	{yybegin(NEW_LINE);}  
   			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>
		{
			{VAR}{SPACE}*\(	{location = location + " " + yytext().replaceAll("\\(","").trim(); parenthese=1;  parameters.clear(); yybegin(PARAM);}
			{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
		    \n             	{yybegin(NEW_LINE);}
			.              	{}
		}
/************************/
/* PARAM        	*/
/************************/
<PARAM>
		{
			{VAR}			{parameters.add(yytext());}
			\(				{parenthese++;}
			\)				{parenthese--;
							 if (parenthese==0) 
							 yybegin(LINE);}
			\&{SPACE}*\n   	{}
			\n     	    	{yybegin(NEW_LINE);}
			.				{}
		}
		
/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL> 
		{
		 	{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
 			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}
		
/************************/
/* WAIT			        */
/************************/
<WAIT>
		{
			\:\:			{yybegin(DECLARATION);}
			"dimension"		{error = false; yybegin(VERIFICATION_DIM);}
			\n				{yybegin(NEW_LINE);}
			.				{}
		}

/************************/
/* WAIT_2			        */
/************************/
<WAIT_2>
		{
			\:\:			{yybegin(VERIF_PARAM);}
			\n				{yybegin(NEW_LINE);}
			.				{}
		}
		
/************************/
/* DECLARATION          */
/************************/	
<DECLARATION>
		{
			{VAR}{SPACE}*\(	{if(parameters.contains(yytext().replaceAll("\\(", "").trim())) { parenthese=1; yybegin(VERIFICATION);} }
			\n				{yybegin(NEW_LINE);}
			.				{}
		}
		
/************************/
/* VERIF_PARAM          */
/************************/	
<VERIF_PARAM>
		{
			{VAR}			{if (parameters.contains(yytext())&&error) setError(location,"the dimention's array must be declared as parameters' function.", yyline+1);}
			\n				{yybegin(NEW_LINE);}
			.				{}
		}
		
/************************/
/* VERIFICATION_DIM     */
/************************/				
<VERIFICATION_DIM>
		{
			{VAR}			{if (!parameters.contains(yytext())&&(location.toLowerCase().contains("function")||location.toLowerCase().contains("subroutine")))
								error = true;
							}
			{INT}			{if(location.toLowerCase().contains("function")||location.toLowerCase().contains("subroutine"))
								error = true;}
			\:				{}
			\(				{parenthese++;}
			\)				{parenthese--;
							 if (parenthese==0) 
							 yybegin(WAIT_2);}
			\n             	{yybegin(NEW_LINE);}
			.				{}
			
		}			


/************************/
/* VERIFICATION         */
/************************/			
<VERIFICATION>
		{
			
			{VAR}			{if (!parameters.contains(yytext())&&(location.toLowerCase().contains("function")||location.toLowerCase().contains("subroutine")))
							setError(location,"the dimention's array must be declared as parameters' function.", yyline+1);}									
			{INT}			{if (location.toLowerCase().contains("function")||location.toLowerCase().contains("subroutine"))
							    setError(location,"the dimention's array must be declared as parameters' function.", yyline+1);}
			\:				{}
			\(				{parenthese++;}
			\)				{parenthese--;
							 if (parenthese==0) 
							 yybegin(DECLARATION);}
			\n             	{yybegin(NEW_LINE);}
			.				{}
		}			


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>
		{
		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
		 	{TYPE}         	{location = yytext(); yybegin(NAMING);}
		 	{DATA_TYPE}		{yybegin(WAIT);}
			{SPACE}			{}
		 	\n             	{}
		  	.              	{yybegin(LINE);}
		}


/************************/
/* LINE STATE           */
/************************/
<LINE>
		{
		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
  			{TYPE}         	{location = yytext(); yybegin(NAMING);}
			{SPACE}			{}
      		\n             	{yybegin(NEW_LINE);}
      		.              	{}
      	}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}

