/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/****************************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.BooleanExpression rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	      		*/
/* As many comments have been done on the ExampleRule.lex file, this file         		*/
/* will restrain its comments on modifications.								     		*/
/*																			      		*/
/****************************************************************************************/

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

%class COMFLOWBooleanExpression
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, CONDITIONAL, LOOP

COMMENT_WORD = \#
FUNCTION         = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

IF			 = "if" | "elif"
THEN		 = "then"
WHILE		 = "while" | "until"
DO			 = "do" 
BOOL		 = \|\|		| \&\&	| \-"o"	| \-"a"
																
%{
	String location = "MAIN PROGRAM";
	int booleanExpressions = 0;

    public COMFLOWBooleanExpression() {
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
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();}
			    {STRING}		{}
			    {IF}			{booleanExpressions=0; yybegin(CONDITIONAL);}
			    {WHILE}			{booleanExpressions=0; yybegin(LOOP);}
			    {VAR}			{} /* Clause to match with words that contains "kill" */
			 	.              	{}
		}
		
/************************/
/* CONDITIONAL STATE    */
/************************/
<CONDITIONAL>   	
		{
				{BOOL}			{booleanExpressions++;
								 if (booleanExpressions==5) setError(location,"It is not allowed use five or more conditional expressions in the same instruction.", yyline+1);}
				\; | {THEN}		{booleanExpressions=0; yybegin(YYINITIAL);}
				{VAR}			{}
				\n | .         	{}  
		}
		
/************************/
/* LOOP STATE	    */
/************************/
<LOOP>   	
		{
				{BOOL}			{booleanExpressions++;
								 if (booleanExpressions==5) setError(location,"It is not allowed use five or more conditional expressions in the same instruction.", yyline+1);}
				\; | {DO}		{booleanExpressions=0; yybegin(YYINITIAL);}
				{VAR}			{}
				\n | .         	{}  
		}


/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}