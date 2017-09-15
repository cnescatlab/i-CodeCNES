/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.ERR.Allocate rule	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;

import java.util.List;

import org.eclipse.core.runtime.Path;




import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F90ERRAllocate
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
%state COMMENT, NAMING, NEW_LINE, LINE, ALLOCATE, ALL_CHECK, ALLOCATED

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
STAT 		 = "stat"{SPACE}*\={SPACE}*{VAR}
ALLOCATE  	 = allocate   | ALLOCATE
DEALLOCATE	 = deallocate | DEALLOCATE
ALLOCATED	 = allocated  | ALLOCATED
ALLOC		 = ({ALLOCATE}|{DEALLOCATE}) {SPACE}* \(
IF		  	 = if		  | IF
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
    private String parsedFileName; 
  	String stat = "";
	boolean ifFound = false;
	String descr = "";
	
	public F90ERRAllocate() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
  
 return getCheckResults(); 
%eofval}


%%          


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
<YYINITIAL>		{ALLOC}			{descr = yytext().toUpperCase(); yybegin(ALLOCATE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{ALLOC}			{descr = yytext().toUpperCase(); yybegin(ALLOCATE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{ALLOC}			{descr = yytext().toUpperCase(); yybegin(ALLOCATE);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* ALLOCATE STATE       */
/************************/
<ALLOCATE>		{STAT}			{stat=yytext().split("=")[1].trim();}
<ALLOCATE>		\n				{yybegin(ALL_CHECK);}
<ALLOCATE>		.				{}


/************************/
/* ALL_CHECK STATE      */
/************************/
<ALL_CHECK>		{IF}			{yybegin(ALLOCATED);}
<ALL_CHECK>		{ALLOC}			{setError(location,"The status of the ALLOCATE or DEALLOCATE instruction is not checked", yyline);
								 descr = yytext().toUpperCase(); yybegin(ALLOCATE);}
<ALL_CHECK>		{VAR}			{}
<ALL_CHECK>		{SPACE}			{}
<ALL_CHECK>		\n				{setError(location,"The status of the ALLOCATE or DEALLOCATE instruction is not checked", yyline);
								 stat = ""; descr = ""; yybegin(NEW_LINE);}
<ALL_CHECK>		.				{}
								 
/************************/
/* ALLOCATED STATE      */
/************************/
<ALLOCATED>		{ALLOCATED}		{stat = ""; yybegin(COMMENT);}
<ALLOCATED>		{VAR}			{if(yytext().equals(stat)) {stat = ""; yybegin(COMMENT);}}	
<ALLOCATED>		\n				{setError(location,"The status of the ALLOCATE or DEALLOCATE instruction is not checked", yyline); yybegin(NEW_LINE);}
<ALLOCATED>		.				{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }