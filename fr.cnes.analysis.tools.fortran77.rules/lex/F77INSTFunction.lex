/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77INSTFunction rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77INSTFunction
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C		| \*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
OTHERS		 = {PROC}	   | {SUB} | {PROG} | {MOD}
TYPE		 = INTEGER | integer | LOGICAL | logical | CHARACTER | character |
			   REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
			   double[\ ]+precision
END			 = END		 | end
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

																
%{
	String location = "MAIN PROGRAM";
	
	Boolean explicitDeclaration = false;
	
	public F77INSTFunction() {
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

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{FUNC}        	{location = yytext(); 
								 if (!explicitDeclaration) this.setError(location,"It misses the type declaration in FUNCTION header.", yyline + 1);
								 explicitDeclaration=false; 
								 yybegin(NAMING);}
<YYINITIAL>  	{OTHERS}       	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{TYPE}			{explicitDeclaration=true ; yybegin(LINE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{FUNC}         	{location = yytext();
								 if (!explicitDeclaration) this.setError(location,"It misses the type declaration in FUNCTION header.", yyline + 1);
								 explicitDeclaration=false; 
								 yybegin(NAMING);}
<NEW_LINE>  	{OTHERS}       	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{TYPE}			{explicitDeclaration=true;}
<NEW_LINE>		{END}			{yybegin(COMMENT);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{explicitDeclaration=false;
								 yybegin(LINE);}
								 
<LINE>			{STRING}		{}
<LINE>      	{FUNC}         	{location = yytext() ;
								 if (!explicitDeclaration) this.setError(location,"It misses the type declaration in FUNCTION header.", yyline + 1);
								 explicitDeclaration=false; 
								 yybegin(NAMING);}
<LINE>		  	{OTHERS}       	{location = yytext(); yybegin(NAMING);}
<LINE>			{TYPE}			{explicitDeclaration=true;}
<LINE>			{END}			{yybegin(COMMENT);}
<LINE>      	\n             	{explicitDeclaration=false; yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
