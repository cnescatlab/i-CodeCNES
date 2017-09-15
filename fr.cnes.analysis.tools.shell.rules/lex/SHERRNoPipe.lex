/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.ERR.NoPipe rule.	 		  */
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

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;




%%

%class SHERRNoPipe
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, AVOID, CASE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
FILEVAR		 = (\~)?[a-zA-Z\/.][a-zA-Z0-9\_\/.\-]*
FILESTRING	 = \$(\{)?{VAR} (\})? (\_)? | {FILEVAR} | \$\$
PRINTFSTRING = "printf" {SPACE}+ {STRING}
NUMERIC		 = [0-9]+([\.][0-9]+)*
CASE	     = "case"
CASE_OPTION	 = (({STRING}|{VAR}|{NUMERIC})+{SPACE}*[\|]*)*{SPACE}*[\)]
ESAC		 = "esac"
OPT			 = \- [a-zA-Z]+
OPTCOMP		 = {STRING} | \${VAR} | {FILESTRING}
SEDSTRING	 = "sed" {SPACE}+ {OPT}* {SPACE}* {STRING} {OPTCOMP}*
OPTION		 = "set"{SPACE}+\-"o"{SPACE}+"pipefail"
PIPELINE	 = \|{SPACE}*	| \|\&
OR			 = \|\|

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Is the state reached inside a case statement */
	private boolean inCase = false;
	

    public SHERRNoPipe() {
    	
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
				\n             	{
									if(inCase){
										yybegin(CASE);
									}else{
										yybegin(YYINITIAL);
									}
								}  
			   	.              	{}
		}
		
/************************/
/* AVOID STATE	    */
/************************/
<AVOID>   	
		{
			   	[^]         	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{if(inCase){
									yybegin(CASE);
								 }else{
									yybegin(YYINITIAL);
								 }
								}  
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
			    {OPTION}		{yybegin(AVOID);}
				{OR}			{}
				{PRINTFSTRING} | {SEDSTRING}	{}
			    {PIPELINE}		{setError(location,"When the pipe is used in the script the option set -o pipefail is mandatory. ", yyline+1);}
	      		[^]         	{}
	      		{CASE}			{
	      							inCase=true;
	      							yybegin(CASE);}
		}
/************************/
/* CASE STATE	    */
/************************/
<CASE>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			  	{CASE_OPTION}	{}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {OPTION}		{yybegin(AVOID);}
				{OR}			{}
				{PRINTFSTRING} | {SEDSTRING}	{}
			    {PIPELINE}		{setError(location,"When the pipe is used in the script the option set -o pipefail is mandatory. ", yyline+1);}
	      		[^]	        	{}
	      		{ESAC}			{
	      							inCase=false;
	      							yybegin(YYINITIAL);}
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}