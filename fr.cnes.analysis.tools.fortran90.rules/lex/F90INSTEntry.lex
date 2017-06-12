/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Entry rule.     */
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

%class F90INSTEntry
%extends AbstractChecker
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING

COMMENT_WORD = "!"
TYPE		 = "function" | "procedure" | "subroutine" | "program" | "module"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM"; 
	
	public F90INSTEntry() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}	
	
%}

%eofval{ 
	return getCheckResults();
%eofval}

/* Transition word is entry (or ENTRY). This word must not be found. */
/* Whenever it's found, an error is returned.						 */
RULE_WORD = [^a-zA-Z0-9\_]("entry")[^a-zA-Z0-9\_]

%%       
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/   
			{COMMENT_WORD}	{yybegin(COMMENT);}

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	
		{
			\n|\r          	{yybegin(YYINITIAL);}  
			.              	{}
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext();
							 yybegin(COMMENT);}
			\n|\r          	{yybegin(YYINITIAL);}  
			.              	{}
		}
		
/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{RULE_WORD}		{this.setError(location,"The instruction ENTRY is not allowed.", yyline + 1);}
			\n|\r  		    {}
			.              	{}
		}

/*********************/
/*	ERROR THROWN	 */
/*********************/	
			[^]	        {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}