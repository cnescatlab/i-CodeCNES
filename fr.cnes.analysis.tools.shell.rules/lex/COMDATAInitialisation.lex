/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Initialisation rule. */
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
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInitialisation
%extends AbstractChecker
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, WRITE, STRING, FORLOOP, READ

COMMENT_WORD = \#
FUNC         = "function"
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
FILEEXIST	 = \[{SPACE}+{OPTION}{SPACE}+(\")?(\{)?\$(\{)?{VAR}(\})?(\")?
OPTION		 = \- ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "k" |
				   "p" | "r" | "s" | "u" | "w" | "x" | "O" | "G" | "L" |
				   "N" | "S" | "z" | "n")
				   
																
%{
	String location = "MAIN PROGRAM";
	List<String> variables = new ArrayList<String>();

    public COMDATAInitialisation() {
    	/** Initialize list with system variables **/
    	String[] systemVariables = {"BASH", "BASH_ENV", "BASH_SUBSHELL", "BASHPID", "BASH_VERSINFO", "BASH_VERSION", 
    								"CDPATH", "DIRSTACK", "EDITOR", "EUID", "FUNCNAME", "GLOBIGNORE", "GROUPS", "HOME", 
    								"HOSTNAME", "HOSTTYPE", "IFS", "IGNOREEOF", "LC_COLLATE", "LC_CTYPE", "LINENO", 
    								"MACHTYPE", "OLDPWD", "OSTYPE", "PATH", "PIPESTATUS", "PPID", "PROMPT_COMMAND", 
    								"PS1", "PS2", "PS3", "PS4", "PWD", "REPLY", "SECONDS", "SHELLOPTS", "SHLVL", "TMOUT", 
    								"UID" };
		variables.addAll(Arrays.asList(systemVariables));
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
				{VAR}			{location = location + yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  	{yybegin(COMMENT);}
				{FUNC}        		{location = yytext(); yybegin(NAMING);}
			    /** variables initialisation **/
			    {VAR}{SPACE}*\=		{String var = yytext().substring(0,yytext().length()-1).trim();
			    					 variables.add(var);}
			    /** Varible use found **/ 
			    \${VAR}				{String var = yytext().substring(1);
			    					 if(!variables.contains(var)) setError(location,"The variable $" + var + " is used before being initialized." , yyline+1);}
			    "tee" | \>\>		{yybegin(WRITE);}
			    "for"				{yybegin(FORLOOP);}
			    "read"				{yybegin(READ);}
			    {FILEEXIST}			{String var = yytext().replaceAll("\"", "").replaceAll("\\{", "").replaceAll("\\}", "").split("\\$")[1];
			                         variables.add(var);}
			    {VAR}				{}
			    \"					{yybegin(STRING);}
			 	.              		{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<WRITE>   	
		{
				\-{VAR}			{}
				\$(\{)?{VAR}	{String var = yytext().substring(1).replace("{",""); 
								 variables.add(var);}
				\n | \;        	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* FORLOOP STATE	    */
/************************/
<FORLOOP>   	
		{
				{VAR}			{variables.add(yytext()); yybegin(YYINITIAL);}
				\n | \;        	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* STRING STATE	    */
/************************/
<STRING>   	
		{
				\\\$			{}
				\$(\{)?{VAR}	{String var = yytext().substring(1).replace("{",""); 
			    			     if(!variables.contains(var)) setError(location,"The variable $" + var + " is used before being initialized." , yyline+1);}
				\n | \; | \"   	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* READ STATE	    */
/************************/
<READ>   	
		{
				{VAR}			{variables.add(yytext()); }
				\n | \;        	{yybegin(YYINITIAL);}  
			   	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}