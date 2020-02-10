/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.PRES.LengthLine rule.	  */
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

%class COMPRESLengthLine
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BEGINFUNC, STRING_DOUBLE, STRING_SIMPLE

COMMENT_WORD = \#
FUNCT			= {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION    	= "function"
FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE			= [\ \r\t\f]
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR		= ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR	= [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR				= {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
	
	String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;
	int length = 0;
	private Stack<Function> functionStack = new Stack<>();

    public COMPRESLengthLine() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void checkLine() throws JFlexException {
		if(length > 100) 
			setError(location,"There are more than 100 characters in this line.", yyline+1);
		length = 0;
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
				\n             	{checkLine(); yybegin(YYINITIAL);}  
			   	.              	{length+=yytext().length();}
		}
				
		
/************************/
/* NAMING STATE	    	*/
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); 
								 length+=yytext().length(); 
								 functionLine = yyline+1;
								 yybegin(BEGINFUNC);}
				\n             	{checkLine(); yybegin(YYINITIAL);}  
			   	.              	{length+=yytext().length();}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  {length+=yytext().length();yybegin(COMMENT);}
				{STRING_D}		{   
									length+=yytext().length();
									yybegin(STRING_DOUBLE);
								}
				{STRING_S}		{
									length+=yytext().length();
									yybegin(STRING_SIMPLE);
								}
				{FUNCTION}     	{length+=yytext().length(); yybegin(NAMING);}
				{FUNCT}			{length+=yytext().length(); 
								 functionLine = yyline+1;
								 location = yytext().substring(0,yytext().length()-2).trim(); 
								 yybegin(BEGINFUNC);
								}
	      		{FUNCSTART}		{
		      						if(!functionStack.empty()){
		      							if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
		      								functionStack.peek().addStarterRepetition();
		      							}
		      						} 
									length+=yytext().length();
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
									length+=yytext().length();
		      					}							
			    \n				{checkLine();}
	      		.              	{length+=yytext().length();}
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
				\(\)			{length+=yytext().length();}
				{FUNCSTART}		{
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
									length+=yytext().length();
								 	yybegin(YYINITIAL);
							 	}
			   	[^]|{SPACE}  {length+=yytext().length();}
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