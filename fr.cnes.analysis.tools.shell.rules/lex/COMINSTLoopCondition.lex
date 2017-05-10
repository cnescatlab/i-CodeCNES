/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.LoopCondition rule.  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTLoopCondition
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, LOOPCONDITION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\'   | \"[^\"]*\"

EQUAL		 = "-eq"		| \=		| \=\=
DIFF		 = "-ne"		| \!\=	
INEG		 = 	\<			| \>		| \<\=		| \>\=		|
			    "-lt"		| "-gt"		| "-le"		| "-ge" 
COND_ERROR	 = {EQUAL}		| {DIFF}

/** The loop conditions are: WHILE, UNTIL 
  * The FOR loop is a little bit different from other programming languages. 
  * Basically, it let's you iterate over a series of 'words'  within a string. 
  **/
WHILE		 = "while"
UNTIL		 = "until"
LOOP		 = {WHILE}	| {UNTIL} 	
			   


																
%{
	String location = "MAIN PROGRAM";

    public COMINSTLoopCondition() {
    	
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
			
%}

%eofval{
	return getViolations();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();}
			    {STRING}		{}
			    {LOOP}			{yybegin(LOOPCONDITION);}
	      		.              	{}
		}
		
/************************/
/* LOOPCONDITION STATE  */
/************************/
<LOOPCONDITION>
		{
			    {STRING}		{}
			    {INEG}			{}
			    {COND_ERROR}	{setError(location,"Inside a loop condition, it is not allowed equality (-eq, ==) or difference(-ne, !=) comparison." , yyline+1); yybegin(COMMENT);}
			    \; | \n			{yybegin(YYINITIAL);}
	      		.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}