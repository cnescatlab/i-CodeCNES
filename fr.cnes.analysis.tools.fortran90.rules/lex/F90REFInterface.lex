/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.Interface rule. */
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
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90REFInterface
%extends AbstractRule
%public
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
%state COMMENT, NAMING, NEW_LINE, LINE, CALLING

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

COMMENT_WORD = \!
FUNC         = "FUNCTION"
PROC         = "PROCEDURE"
SUB          = "SUBROUTINE"
PROG         = "PROGRAM"
MOD          = "MODULE"
INTERFACE	 = "INTERFACE"
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}  | {INTERFACE}
END			 = "END"
END_TYPE	 = {END} [\ ]* {TYPE} 
CALL		 = "CALL"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
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
 	List<String> call     = new LinkedList<String>();
 	List<String> callFrom = new LinkedList<String>();
 	List<String> locList  = new LinkedList<String>();
 	List<String> allLocs  = new LinkedList<String>();
 	List<Integer> lines   = new LinkedList<Integer>();
 	

	
	public F90REFInterface() {

    }
    
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void checkCalls(String word) {
		for (int i = 0; i < call.size(); i++) {
			if(call.get(i).equals(word) && locList.contains(callFrom.get(i))) {
				call.remove(i);
				callFrom.remove(i);
				lines.remove(i);
			}
		}
	}
	
	/** Print the error that appears in the list **/
	private void printErrors() throws JFlexException {
		/** Only print the functions declared in this file **/
		for (int i = 0; i < call.size(); i++) {
			if(allLocs.contains(call.get(i))) 
				setError(callFrom.get(i),"The function " + call.get(i) + " is not visible in this point.", lines.get(i));
		}
	}

	
%}

%eofval{ 
 printErrors();
  
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
<NAMING>		{VAR}			{String text = yytext().toLowerCase().trim();
								 if(!location.toLowerCase().trim().equals("module") || !text.equals("procedure")) {
								 	location = location + " " + text;
								 	locList.add(location); 
								 	allLocs.add(text);
								 }
								 checkCalls(text);
								 yybegin(COMMENT);}
<NAMING>    	\n             	{locList.add(location); yybegin(NEW_LINE);}
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
<NEW_LINE>		{END_TYPE}		{locList.remove(locList.size()-1);}
<NEW_LINE>		{CALL}			{yybegin(CALLING);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{END_TYPE}		{locList.remove(locList.size()-1);}
<LINE>			{CALL}			{yybegin(CALLING);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* LINE STATE           */
/************************/
<CALLING>	  	{VAR}			{String called = yytext().toLowerCase();
								 /** If this function isdeclared before in the file -> visible -> no error **/
								 if (!allLocs.contains(called) && locList.size()==1) {
								 	call.add(yytext().toLowerCase()); 
								 	callFrom.add(locList.get(locList.size()-1)); 
								 	lines.add(yyline+1); yybegin(COMMENT);
								 }
								}
<CALLING>		\n				{yybegin(NEW_LINE);}
<CALLING>		.				{}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
