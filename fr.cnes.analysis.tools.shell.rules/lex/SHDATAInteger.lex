/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.DATA.Integer rule. 		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHDATAInteger
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, INTDECLARATION, LINE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f\space]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
INT			 = [0-9]+
REAL		 = [0-9]*\.[0-9]+

CONTROL_VAR  = {VAR}({SPACE}+\-{VAR})*
TYPESET		 = "typeset"{SPACE}+\-"i"



																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> integers = new ArrayList<String>();

    public SHDATAInteger() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
			
%}

%eofval{
	return getCheckResults();
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
				{FNAME}			{location = yytext(); yybegin(LINE);}
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
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
			    {TYPESET}		{yybegin(INTDECLARATION);}
			    {VAR}\={REAL}	{}
			    {VAR}\={INT}	{String variable = yytext().split("=")[0];
			    				 if (!integers.contains(variable)) setError(location,"The integer variables must be defined using the typeset -i declaration.", yyline+1);}
				{SPACE}			{}
				{CONTROL_VAR}	{}
	      		\n | \;			{}
	      		. 	         	{yybegin(LINE);}
		}
		
/************************/
/* LINE STATE	    	*/
/************************/
<LINE>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
			    \n				{yybegin(YYINITIAL);}
	      		.	        	{}
		}
		
/************************/
/* INTDECLARATION STATE */
/************************/
<INTDECLARATION>   	
		{
				{VAR}			{integers.add(yytext()); yybegin(COMMENT);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}