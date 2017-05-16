/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.Logical rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHINSTLogical
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, LOGICAL, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

LOGIC		 = \|\|		| \&\&
LOGICCORRECT = "echo"	| "exit"

COND		 = "if"		| "until"	| "for"		| "while"	| "elif"
ENDCOND		 = "do"		| "then"

																
%{
	String location = "MAIN PROGRAM";

    public SHINSTLogical() {
    	
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
/* AVOID STATE	    	*/
/************************/
<AVOID>   	
		{
				{ENDCOND}		{yybegin(YYINITIAL);}
			   	[^]	        	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
			    {COND} 			{yybegin(AVOID);}
			    {LOGIC}			{yybegin(LOGICAL);}
			    {VAR}			{}
	      		[^]         	{}
		}
		
/************************/
/* LOGICAL STATE   		*/
/************************/
<LOGICAL>   	
		{
				{LOGICCORRECT}	{yybegin(YYINITIAL);}
				{VAR}			{setError(location,"The abbreviation || and && must be followed only by ECHO or EXIT.", yyline+1); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}
				{SPACE}			{}  
			   	.              	{setError(location,"The abbreviation || and && must be followed only by ECHO or EXIT.", yyline+1); yybegin(YYINITIAL);}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}