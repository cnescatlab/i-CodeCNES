/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is an example on how to edit a rule with JFlex, in order to fit */
/* with plug-in's definitions. Important point that must not be changed are  */
/* described. Moreover, implementation part that only depend on the rule are */
/* also showed. This example can't be compiled as it only shows how it can   */
/* be used, without using real names. We advise to see JFlex documentation   */
/* and automata theory before reading this example.   						 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.util.LinkedList;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.rules.AbstractChecker; 
import fr.cnes.analysis.tools.analyzer.rules.ViolationIdentifier;

/* All code lines before this marker (%%) are directly written in the 		*/
/* generated Java code, at the beginning. This is were you can declare your */
/* analyzer package (if needed) and all package import you need. LinkedList */
/* import is not meant to be deleted, as well as package name.				*/
%%

/* This is were you declare the name of the generated Java class. You decide */
/* whether it implements or extends a class. To fit with plug-in's 			 */
/* definition, this class must extends AbstractChecker class.					 */
%class GeneratedRuleName
%extends AbstractChecker
%public
%line

/* This three lines are not meant to be modified. */
%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FREE_COMMENT determines the begin of comment which are not at the 	*/
/* start of a line.														*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/
/* These transition's words are not meant to be changed.				*/
COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
/* All this code is not meant to be deleted, but some Java program can be added */
/* in this section.																*/
%{
	String location = "Programme";
	
	
	public GeneratedRuleName() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
return violations;
%eofval}

/* Here is where you can declare transition words for the rule. */
RULE_WORD = first_word | SECOND_WORD

%%          

/* This is the general automaton. Each part will be described later. */

/******************************************************************************/
/* This part deals with a "free" comment, which means not at the beginning of */
/* a line.																	  */
/******************************************************************************/
				{FREE_COMMENT}	{yybegin(COMMENT);}
				
/******************************************************************************/
/* This part deals with the comment section, to avoid any word on these lines.*/
/******************************************************************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/******************************************************************************/
/* This part deals with function, procedure and other's name. It is 	 	  */
/* to determine, when it exists, the name of each one. Whenever a name  	  */
/* is encountered, the following part is ignored, moving to COMMENT state.    */
/******************************************************************************/
<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
								 
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/******************************************************************************/
/* This is the first state of the automaton. The automaton will never go back */
/* to this state after.                                                       */
/******************************************************************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext();
								 yybegin(NAMING);}

/* This what has to be done whenever an error is found. */
<YYINITIAL>		{RULE_WORD}		{this.setError(location, message, yyline + 1);
								 yybegin(LINE);}

<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/******************************************************************************/
/* This state is reached whenever a new line starts.                          */
/******************************************************************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}

/* This what has to be done whenever an error is found. */
<NEW_LINE>		{RULE_WORD}		{this.setError(location,message, yyline + 1);
								 yybegin(LINE);}

<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

/******************************************************************************/
/* This state is whenever none of the others has been reached.                */
/******************************************************************************/
<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext();
								 yybegin(NAMING);}


/* This what has to be done whenever an error is found. */
<LINE> 			{RULE_WORD}		{this.setError(location,message, yyline + 1);}

<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

/******************************************************************************/
/* An error is thrown if nothing is catch before this line. 				  */
/******************************************************************************/
				[^]           {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
