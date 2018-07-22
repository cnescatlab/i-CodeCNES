/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/****************************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.CaseSwitch rule.			*/
/* For further information on this, we advise you to refer to RNC manuals.	      		*/
/* As many comments have been done on the ExampleRule.lex file, this file         		*/
/* will restrain its comments on modifications.								     		*/
/*																			      		*/
/****************************************************************************************/

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

%class COMFLOWCaseSwitch
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BEGINFUNC, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION     = "function"
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f\space]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

CASE		 = "case"
ESAC		 = "esac"
																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
	
	String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

	private String parsedFileName;
	boolean defaultExpr = false;
	
	private Stack<Function> functionStack = new Stack<>();


    public COMFLOWCaseSwitch() {
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
				{FNAME}			{location = yytext(); functionLine = yyline+1; yybegin(BEGINFUNC);}
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
 				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
			    {CASE}			{defaultExpr=false;}
				\*\)			{defaultExpr=true;}
				{ESAC}			{if(!defaultExpr) setError(location,"The default case of the case switch condition is missing.", yyline+1); 
								 defaultExpr=false;}
			    {VAR}			{} /* Clause to match with words that contains "kill" */
			 	[^]            	{}
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
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}