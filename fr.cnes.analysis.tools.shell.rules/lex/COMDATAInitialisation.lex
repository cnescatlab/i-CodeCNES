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
import java.util.EmptyStackException;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class COMDATAInitialisation
%extends AbstractChecker
%public
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, WRITE, STRING, FORLOOP, READ, BEGINFUNC

COMMENT_WORD = \#
FUNCT			= {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION    	= "function"
FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SPACE			= [\ \r\t\f\space]
SHELL_VAR		= ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR	= [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR				= {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "done"


FILEEXIST	 = \[{SPACE}+{OPTION}{SPACE}+(\")?(\{)?\$(\{)?{VAR}(\})?(\")?
OPTION		 = \- ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "k" |
				   "p" | "r" | "s" | "u" | "w" | "x" | "O" | "G" | "L" |
				   "N" | "S" | "z" | "n")
				   
																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
    /* ERROR_ON_VARIABLE: constant for variable error message */
    private static final String ERROR_ON_VARIABLE = " -> Error with the variable named ";
    /* LOCAL_VAR_SAME_NAME: constant for local variable message */
    private static final String LOCAL_VAR_SAME_NAME = ". There is a local variable with the same name.";
    /* FUNCT_SAME_NAME: constant for function name message */
    private static final String FUNCT_SAME_NAME = ". There is a function with the same name.";

	private Stack<Function> functionStack = new Stack<>();
	
    /* location: the current function name, or main program, that is the initial value */
    private String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

	/* parsedFileName: name of the current file */
	private String parsedFileName;

	
    /* functions: the list of function names in the code analyzed so far */
    private List<String> functions = new ArrayList<String>();
    /* localVariables: the list of local variables in the code analyzed so far */
    private List<String> localVariables = new ArrayList<String>();
    /* globalVariables: the list of global variables in the code analyzed so far */
    private List<String> globalVariables = new ArrayList<String>();
    /* currentLocals: the list of local variables in the current function */
    private List<String> currentLocals = new ArrayList<String>();
    /* extraGlobals: the list of the local variables of an encapsulating function */
    private List<String> extraGlobals = new ArrayList<String>();
    
    /* currentOpening: the opening type of the current function */
    private String currentOpening = "";
    /* nbOpenings: the nuber of times the current opening has been used in the function number */ 
    private int nbOpenings = 0;
    /* doneOpenings: a list containing the "done" type family of openings */
    private final List<String> doneOpenings = new ArrayList<String>(Arrays.asList("select", "for", "while", "until"));

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
		this.parsedFileName = file.toString();
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void endLocation() throws JFlexException {
		try{
		    Function functionFinished = functionStack.pop();
			if (!functionStack.empty()) {
				/* there is a current function: change location to this function */
				location = functionStack.peek().getName();
			} else {
				/* we are in the main program: change location to main */
				location = MAINPROGRAM;
			}
		}catch(EmptyStackException e){
        		final String errorMessage = e.getMessage();
            	throw new JFlexException(this.getClass().getName(), parsedFileName,
        errorMessage, yytext(), yyline, yycolumn);
		}
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
				{VAR}			{
									location = yytext();
									functionLine = yyline+1;
									yybegin(BEGINFUNC);
								}
				\n             	{
									yybegin(YYINITIAL);
								}  
			   	.			    {}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  	{yybegin(COMMENT);}
				{FUNCTION}        	{yybegin(NAMING);}
				{FUNCT}				{
										functionLine = yyline+1;
										location = yytext().substring(0,yytext().length()-2).trim();
									 	yybegin(BEGINFUNC);
								 	}
	      		{FUNCSTART}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
		      									functionStack.peek().addStarterRepetition();
		      								}
		      							} 		      							
		      						}
	      		{FUNCEND}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().isFinisher(yytext())){
		      									if(functionStack.peek().getStarterRepetition()>0) {
	      										    functionStack.peek().removeStarterRepetition();
		      									} else {
		      										endLocation();
		      									}
		      								}
										}
		      						}							
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
/* BEGINFUNC STATE	    */
/************************/
/*
 * This state target is to retrieve the function starter. For more information on fonction starter, have a look on {@link Function} class.
 * Pending this starter, the function ender can be defined.
 *
 */ 
<BEGINFUNC>
		{
				\(\)			{}
				{FUNCSTART}		{
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
								 	yybegin(YYINITIAL);
							 	}
			   	[^]|{SPACE}  {}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}