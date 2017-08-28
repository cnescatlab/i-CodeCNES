/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.IO rule. 		 */
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

%class F90DESIGNIO
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, OPEN

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
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}  | {INTERFACE}
NEWUNIT		 = {VAR} [\ ]* \= [\ ]* {VAR}
OPEN		 = "OPEN"
UNIT		 = "UNIT"
OPEN_DEC	 = {OPEN} [\ ]* \( 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
INT			 = [0-9]+
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
    private String parsedFileName; 
 	List<String> unites = new LinkedList<String>();
	
	public F90DESIGNIO() {
    }
    
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void addUnit (String word){
		// word id like <<VAR_X = VAR_Y>>
		String unit = word.split("=")[0].trim();
		if(!unites.contains(unit)) unites.add(unit);
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
/* NAMING STATE        	*/
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext();  yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
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
<NEW_LINE>		{NEWUNIT}		{addUnit(yytext());}
<NEW_LINE>		{OPEN_DEC}		{yybegin(OPEN);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<LINE>			{STRING}		{yybegin(LINE);}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{NEWUNIT}		{addUnit(yytext());}
<LINE>			{OPEN_DEC}		{yybegin(OPEN);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* OPEN STATE           */
/************************/
<OPEN>			{UNIT}			{}
<OPEN>			{INT}			{setError(location,"The value of the logic unity should be a integer or a variable initialised directly.", yyline+1); yybegin(LINE);}
<OPEN>		  	{VAR} 			{if (!unites.contains(yytext())) setError(location,"The value of the logic unity should be a integer or a variable initialised directly.", yyline+1);
								 yybegin(LINE);}
<OPEN>			\n				{}
<OPEN>			.				{}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }