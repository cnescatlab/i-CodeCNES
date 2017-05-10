/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.BoolNegation rule.   */
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

%class COMINSTBoolNegation
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, LOGICAL

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

NOT			 = \!
OPER		 = \&\&	  |  \|\|   | \-"o"	 |  \-"a" 


																
%{
	String location = "MAIN PROGRAM";
	/** Bool to knkow if there are open brackets **/
	int bracket = 0, brace = 0, parenth = 0;

    public COMINSTBoolNegation() {
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
				{NOT}			{bracket=0; brace=0; parenth=0; yybegin(LOGICAL);}
			    {STRING}		{}
			 	.              	{}
		}
		
/************************/
/* LOGICAL STATE   	    */
/************************/
<LOGICAL>
		{
				\[				{bracket++;}
				\]				{bracket--;}
				\{				{brace++;}
				\}				{brace--;}
				\(				{parenth++;}
				\)				{parenth--;}
				{NOT} | "\-ne"	{setError(location,"Double negation is not allowed.", yyline+1); yybegin(COMMENT);}
				{OPER}			{if(bracket <= 0 && brace <= 0 && parenth <=0) yybegin(YYINITIAL);}
		      	\n             	{yybegin(YYINITIAL);}
		      	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}