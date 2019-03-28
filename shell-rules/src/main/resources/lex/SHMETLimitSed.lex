/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.MET.LimitAWK rule. 		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class SHMETLimitSed
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, SED, SED_LINES

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_\/]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

OPTION		 = "-e"		| "--expression" 	| "-f"		| "--file"		|
			   "-n"		| "--quiet"			| "--silent"				|
			   "-h"		| "--help"			| "-v"		| "--version"

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	int actions=0, linesSed=0;
	int lineError=0;
	
    public SHMETLimitSed() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
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
				{FNAME}			{location = yytext(); yybegin(YYINITIAL);}
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
			    "sed"			{lineError=yyline+1; actions=0; yybegin(SED);}
			    {VAR}			{}
	      		[^]	        	{}
		}
		
/************************/
/* SED STATE		    */
/************************/
<SED>   	
		{
				{OPTION}		{actions++;}
				";"				{if (actions==0) actions = 1; /* a ";" means there is an action before it, so +1 */
								 actions++;}
				\'				{linesSed=0; yybegin(SED_LINES);}
				\n | \|			{if(actions>5) setError(location,"The SED expression has more than 5 actions", lineError); yybegin(YYINITIAL);}
				\\{SPACE}*\n	{}
				.	         	{}  
		}
		
/************************/
/* SED_LINES STATE	    */
/************************/
<SED_LINES>   	
		{
				\'				{if(linesSed>5) setError(location,"The SED expression has more than 5 lines", lineError); yybegin(SED);}
				\n				{linesSed++;}
				.	         	{}  
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}