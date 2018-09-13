/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.MET.PipeLine rule. 		  */
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
import java.util.EmptyStackException;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class SHMETPipeLine
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, AVOID, CONSUME_LINE, BEGINFUNC, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]

VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
PIPELINE	 = \|{SPACE}	| \|\n		| \|\&

CASE_STMT	 = [^"\n""("" ""\r""\f""#"][^"\n""("]*\)

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";

	private Stack<Function> functionStack = new Stack<>();
	
    /* location: the current function name, or main program, that is the initial value */
    private String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;
	int bracket = 0;
	String type = "";
	/** For each line a string that represent the line type: comment, empty, line **/
	List<String> linesType = new ArrayList<String>();

    public SHMETPipeLine() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
		
	/**
	 * Check the line before to the current line
	 * @param index
	 * @param lineError
	 * @throws JFlexException
	 */
	private void checkPrecedent(int index, int lineError) throws JFlexException {
		if(index>0) {
			if (linesType.get(index-1).equals("empty")) checkPrecedent(index-1,lineError);
			else if (linesType.get(index-1).equals("line")) setError(location,"Every pipeline must be preceded by a comment.", lineError);
			else if (linesType.get(index-1).equals("pipeline")) setError(location,"Every pipeline must be preceded by a comment.", lineError);
		}
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
				\!\/"bin"		{type="line";}
				\n             	{linesType.add(type); type="empty"; yybegin(YYINITIAL);}  
			   	.              	{if(type.equals("empty")) type="comment";}
		}
		
		
/************************/
/* NAMING STATE	   		*/
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); functionLine = yyline+1; yybegin(BEGINFUNC);}
				\n             	{linesType.add("line"); type="empty"; yybegin(YYINITIAL);}  
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
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); 
								 if(type.equals("empty")) type="line";
								 functionLine = yyline+1;
								 yybegin(BEGINFUNC);
								}
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
			    \|\|			{}	/** OR logique **/
				{CASE_STMT}		{}  /* a case statement can contain multiple choices with | -> ignore */
			    {PIPELINE}		{type="pipeline";
								 linesType.add(type); 
								 type="empty";
								 int index = linesType.size()-1;
								 checkPrecedent(index, yyline+1);
								 yybegin(CONSUME_LINE);
								}
				{SPACE}			{}
	      		\n             	{linesType.add(type); type="empty";}
	      		\\{SPACE}*\n	{}
	      		.				{if(type.equals("empty")) type="line";}
		}

/************************/
/* CONSUME_LINE STATE	    */
/************************/
/* Consume characters till end of line */
<CONSUME_LINE>
		{
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
				\n				{yybegin(YYINITIAL);}
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
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}