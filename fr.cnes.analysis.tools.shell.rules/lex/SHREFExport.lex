/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.REF.Export rule.	 		  */
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
import java.util.EmptyStackException;
import java.util.Stack;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class SHREFExport
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BEGINFUNC, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f\space]
NAME	     = [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR	 = ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR = [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR			 = {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

EXPORT		 = "export"{SPACE}+\-"f"{SPACE}+{FNAME}
																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";

	String location = MAINPROGRAM;
 	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;
	int length = 0;
	private Stack<Function> functionStack = new Stack<>();

    public SHREFExport() {
    	
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
				{FNAME}			{location = yytext(); 
								 functionLine = yyline+1;
								 yybegin(BEGINFUNC);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{STRING_D}		{   
									length+=yytext().length();
									yybegin(STRING_DOUBLE);
								}
				{STRING_S}		{
									length+=yytext().length();
									yybegin(STRING_SIMPLE);
								}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{functionLine = yyline+1;
								 location = yytext().substring(0,yytext().length()-2).trim(); 
								 yybegin(BEGINFUNC);}
				{FUNCSTART}		{
		      						if(!functionStack.empty()){
		      							if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
		      								functionStack.peek().addStarterRepetition();
		      							}
		      						} 
		      					}
	      		{FUNCEND}		{
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
			    {EXPORT}		{setError(location,"It is forbidden to export functions.", yyline+1);}
	      		[^]         	{}
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

/*
 * The string states are designed to avoid problems due to patterns found in strings.
 */ 
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
				{IGNORE_STRING_S}	{length+=yytext().length();}
				{STRING_S}    		{
										length+=yytext().length();
										yybegin(YYINITIAL);
									}  
		  	 	[^]|{SPACE}  		{length+=yytext().length();}
		}

/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
				{IGNORE_STRING_D}	{length+=yytext().length();}
				{STRING_D}    		{
										length+=yytext().length();
										yybegin(YYINITIAL);
									}  
		  	 	[^]|{SPACE}  		{length+=yytext().length();}
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}