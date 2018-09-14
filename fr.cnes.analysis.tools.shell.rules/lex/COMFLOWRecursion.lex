/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Recursion rule.  	  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.EmptyStackException;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class COMFLOWRecursion
%extends AbstractChecker
%public
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BEGINFUNC, STRING_DOUBLE, STRING_SIMPLE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
STRING		 = \'[^\']*\' | \"[^\"]*\"
IGNORE		 = "EOF" [^]* "EOF"

FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE			= [\ \r\t\f]
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR		= ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR	= [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR				= {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))

OPTCHAR		 = \# | \! | % | \* | @ | \^ | \' | , | \/ | : | = | \+ | \?
EXTENDEDVAR	 = \$\{ {OPTCHAR}* {VAR} {OPTCHAR}* {VAR}? (\[)? {OPTCHAR}* (\])? \}

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
	int brackets = 0;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;
	
    /** Map that contains all the functions with its calls **/
	Map<String,List<String>> functionCalls = new HashMap<String,List<String>>();
	/** Current list of calls **/
	List<String> calls = new ArrayList<String>();

    private String parsedFileName;
	private Stack<Function> functionStack = new Stack<>();

    public COMFLOWRecursion() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/** 
	  * This function checks if there are circular function calling using the 
	  * lost 'functionCalling' and throw an error when it finds one
	  *
	  * @param
	  *     var: string called inside the current function 
	  */
	private Boolean checkCircularCalling(String var) throws JFlexException {
		List<String> callings = functionCalls.get(var);
		if (callings!=null) {
			if (callings.contains(location)) {
				setError(location,"The use of recursivity is not allowed.", yyline+1);
				return true;
			}				
			else {
				Boolean found = false;
				for (String element : callings) {
					found = checkCircularCalling(element);
					if (found) return true;
				}
			}
		}
		return false;
	}
	
	private void endLocation() throws JFlexException {
		try{
		    Function functionFinished = functionStack.pop();
			List<String> list = new ArrayList<String>(calls);
			functionCalls.put(location,list);
			calls.clear();

			if (!functionStack.empty()) {
				/* there is a current function: change location to this function */
				location = functionStack.peek().getName();
				List<String> currentCalls = functionCalls.get(location);
				if (currentCalls != null) 
				{
					calls.addAll(currentCalls);
				} 
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
				{FNAME}			{location = yytext(); yybegin(BEGINFUNC);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); brackets=0; yybegin(BEGINFUNC);}
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
				{VAR}			{/** call to the same function **/
								 if(yytext().equals(location)) setError(location,"The use of recursivity is not allowed.", yyline+1);
								 /** save in list to verify circular calling **/
								 else {
									if (!functionStack.empty()){
										// we are in a function
										checkCircularCalling(yytext());
										if (!calls.contains(yytext()))
											calls.add(yytext());
									}
								 }
								}
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

		
/*
 * The string states are designed to avoid problems due to patterns found in strings.
 */ 
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
				{IGNORE_STRING_S}	{}
				{STRING_S}    		{yybegin(YYINITIAL);}  
		  	 	[^]|{SPACE}  		{}
		}

/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
				{IGNORE_STRING_D}	{}
				{STRING_D}    		{yybegin(YYINITIAL);}  
		  	 	[^]|{SPACE}  		{}
		}

		

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}