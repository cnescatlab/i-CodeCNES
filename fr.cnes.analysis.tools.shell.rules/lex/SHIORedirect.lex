/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**
 * This file is used to generate a rule checker for SH.IO.Redirect rule. 
 * For further information on this, we advise you to refer to RNC manuals.
 * Redirection based on GNU redirect manual page.
 *
 * The different redirection are :
 *
 * - Input redirection (e.g : [n]<word )
 * - Output redirection (e.g : [n]>[|]word
 * - Redirecting standard ouput or standard Error (e.g : &>word , >&word,  >word 2>&1
 * - Appending standard output or standard Error (e.g : 
 *
 *
 * Usual operators :
 * - >&-
 * - <&-
 * - >
 * - <
 * - >>
 * - <<
 * - <<<
 * - >>>
 * - <&digit-
 * - >&digit-
 * - <>
 */
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

%class SHIORedirect
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BEGINFUNC

COMMENT_WORD	 = \#
FUNCTION    	 = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     	= [a-zA-Z][a-zA-Z0-9\_]*
STRING		 	= \'[^\']*\' | \"[^\"]*\"
OPERATOR_RIGHT  = [\>]|[\>][\&]|[\&][\>]|[\>][\>]|[\>][\>][\>]
OPERATOR_LEFT	= [\<]|[\<][\&]|[\&][\<]|[\<][\<]|[\<][\<][\<]
OPERATOR_RL		= [\<][\>]
REDIRECT_RIGHT	= ([a-zA-Z0-9\_\?\-\/\\\}]|{STRING})+{SPACE}*({OPERATOR_RIGHT}|{OPERATOR_RL}){SPACE}*([a-zA-Z3-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT_LEFT	= ([a-zA-Z3-9\_\?\-\/\\\}]|{STRING})+{SPACE}*({OPERATOR_LEFT}|{OPERATOR_RL}){SPACE}*([a-zA-Z0-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT_RL		= ([a-zA-Z3-9\_\?\-\/\\\}]|{STRING})+{SPACE}*{OPERATOR_RL}{SPACE}*([a-zA-Z3-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT 		= {REDIRECT_RIGHT}|{REDIRECT_LEFT}|{REDIRECT_RL}

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"
																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
	
	String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;
	List<String> redirections = new ArrayList<String>();
	private boolean isLastLineCommented = false;
	private boolean errorReported=false;
	
	private Stack<Function> functionStack = new Stack<>();
	
    public SHIORedirect() {
    	
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
				\n             	{
									yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); functionLine = yyline+1; yybegin(BEGINFUNC);}

				\n             	{	
									isLastLineCommented=false;
									yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{
			  						isLastLineCommented = true;
			  						yybegin(COMMENT);
		  						}
			    {REDIRECT}		{if (!isLastLineCommented && !errorReported){
			    					errorReported=true;
			    					setError(location,"The non-standard redirection "+yytext()+" must be preceded by a comment.", yyline+1);
			    				}
			    				}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{functionLine = yyline+1;
								 location = yytext().substring(0,yytext().length()-2).trim();
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
				{STRING}		{}
	      		.         		{}
	     		\n				{isLastLineCommented=false;errorReported=false;}
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
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}