/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.MET.LimitAWK rule. 		  */
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

%class SHMETLimitSed
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, SED, SED_LINES

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_\/]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

OPTION		 = "-e"		| "--expression" 	| "-f"		| "--file"		|
			   "-n"		| "--quiet"			| "--silent"				|
			   "-h"		| "--help"			| "-v"		| "--version"

																
%{
	String location = "MAIN PROGRAM";
	int actions=0, linesSed=0;
	int lineError=0;
	
    public SHMETLimitSed() {
    	
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
				[^]            {}