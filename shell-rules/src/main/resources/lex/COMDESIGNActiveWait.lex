/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DESIGN.ActiveWait rule.   */
/* For further information on this, we advise you to refer to RNC manuals.        */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.                                   */
/*                                                                                */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;
import java.util.EmptyStackException;
import java.util.Stack;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.shell.Function;

%%

%class COMDESIGNActiveWait
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
SPACE		 = [\ \r\t\f]
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
ASSIGNMENT   = {VAR}"="
OPTION       = "-"[a-zA-Z][a-zA-Z0-9\_\-]*

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

NOT_ACTWAIT     = "while"{SPACE}*{ASSIGNMENT}*{SPACE}*read{SPACE}*{OPTION}*{SPACE}*{VAR}
ACTWAIT_WHILE   = "while"{SPACE}*\[{SPACE}*"1"{SPACE}*\]{SPACE}* 
ACTWAIT		    = "read" | "sleep" | "wait"
                                                                
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
	
	String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;

	private Stack<Function> functionStack = new Stack<>();

    public COMDESIGNActiveWait() {
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
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
%eofclose

%%          



/************************/



/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>       
        {
                \n              {yybegin(YYINITIAL);}  
                .               {}
        }
        
/************************/
/* NAMING STATE     */
/************************/
<NAMING>    
        {
                {FNAME}         {location = yytext(); functionLine = yyline+1; yybegin(BEGINFUNC);}
                \n              {yybegin(YYINITIAL);}  
                .               {}
        }

/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>
        {
                {COMMENT_WORD}  {yybegin(COMMENT);}
				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
 				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{functionLine = yyline+1;
								 location = yytext().substring(0,yytext().length()-2).trim();
								 yybegin(BEGINFUNC);}
				{NOT_ACTWAIT}   {
				                    if(!functionStack.empty()){
                                        if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
                                            functionStack.peek().addStarterRepetition();
                                        }
                                    }
				                }
				{ACTWAIT}       {setError(location,"There is an active wait in this point.", yyline+1); }
				{ACTWAIT_WHILE} {
									setError(location,"There is an active wait in this point.", yyline+1); 
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									}
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
                {VAR}           {} /* Clause to match with words */
                [^]             {}
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
				{NOT_ACTWAIT}   {
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
								 	yybegin(YYINITIAL);
								}
				{ACTWAIT_WHILE} {
									setError(location,"There is an active wait in this point.", yyline+1); 
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
								 	yybegin(YYINITIAL);
								}
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
/* ERROR STATE          */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}
