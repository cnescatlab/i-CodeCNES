/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.FLOW.CheckCodeReturn rule. */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
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

%class SHFLOWCheckCodeReturn
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CHECKRET, FINLINE, COMMENTARGS, PIPELINE, CONSUMECOMMAND, BEGINFUNC, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
SPACE		 = [\ \r\t\f]
FUNCTION	 = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

PIPEIGNORE 	 = "||"
PIPE	     = \|

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
	/** Map with each function name and if contains a return or not **/
	private Map<String,Boolean> functions = new HashMap<String,Boolean>();
	/** The return of the last function is checked: avoid problems with last call **/
	private boolean verified = false;
	private boolean functionCalled = false;
	private boolean pipeline = false;
	/** Number of lines since last function call **/
	private int linesError = 1;
	/** Last function called **/
	private String functionCall = "";

	private Stack<Function> functionStack = new Stack<>();

    public SHFLOWCheckCodeReturn() {
    	/** The command 'cd' must be checked as the functions **/
		functions.put("awk",true);
		functions.put("batch",true);
		functions.put("cat",true);
    	functions.put("cd",true);
		functions.put("chgrp",true);
		functions.put("chmod",true);
		functions.put("chown",true);
		functions.put("command",true);
		functions.put("compress",true);
		functions.put("cp",true);
		functions.put("expr",true);
		functions.put("find",true);
		functions.put("kill",true);
		functions.put("ls",true);
		functions.put("mkdir",true);
		functions.put("mv",true);
		functions.put("read",true);
		functions.put("rm",true);
		functions.put("rmdel",true);
		functions.put("rmdir",true);
		functions.put("sed",true);
		functions.put("touch",true);
		functions.put("uncompress",true);
		functions.put("write",true);
		functions.put("xargs",true);
    }
	
    /** 
      * addViolation: adds a violation on the function named functionCall
      * The violation is added either when there is a new function call, or 
	  * when the EOF is reached, so is stored, along with the number of lines ago(linesError).
	  * If the violation is on a pipeline, functionCall contains all the functions of the  
	  * pipeline that are in the functions list.
      */
    private void addViolation() throws JFlexException {
        if(pipeline == true) {
            setError(location, "The return status of pipeline with function(s) " + functionCall + " has not been checked.", yyline+1-linesError);
            pipeline = false;
        } else {
            setError(location, "The return status of function " + functionCall + " has not been checked.", yyline+1-linesError);
        }
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

/* ------------------------------------------------------------------------------------------------- */
/* The eofval function has been removed to treat EOF differently when found in different states      */
/* This is due to the fact that the violation is raised when at least one of the following lines     */
/* has been parsed. According to what is parsed, the violation needs to be raised on a previous line */
/* or not.																							 */
/* The eofval function MUST NOT be used in combination with EOF recognition in states (exclusive). 	 */
/* ------------------------------------------------------------------------------------------------- */


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
				<<EOF>>			{return getCheckResults();}
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); functions.put(location, false); yybegin(BEGINFUNC);}
				\n             	{yybegin(YYINITIAL);}  
				<<EOF>>			{return getCheckResults();}
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				{PIPEIGNORE}	{pipeline = false;}
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
 				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
				{PIPE}			{pipeline = true;}				
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); functions.put(location, false); yybegin(BEGINFUNC);}
			    {VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
			    				 		functionCall=yytext();
			    				 		yybegin(FINLINE);
			    				} else {
			    					functionCalled=false;
									yybegin(CONSUMECOMMAND);
			    				}} 
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
				<<EOF>>			{return getCheckResults();}
			 	[^]            	{}
		}
		
/************************/
/* FINLINE STATE	    */
/************************/
<FINLINE>   	
		{
				{PIPEIGNORE}	{pipeline = false; yybegin(YYINITIAL);}
				{PIPE}			{pipeline = true; yybegin(PIPELINE);}
				\n				{yybegin(CHECKRET);}
				<<EOF>>			{   
									linesError=0;
									if(!verified && functionCalled) addViolation();
									return getCheckResults();
								}
			   	.              	{}
		}

/************************/
/* PIPELINE STATE	    */
/************************/
<PIPELINE>   	
		{
				{VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
										if (functionCall.length() != 0) functionCall+=", ";
			    				 		functionCall+=yytext();
			    				}} 
				\n				{yybegin(CHECKRET);}
				<<EOF>>			{    
									linesError=0;
									if(!verified && functionCalled) addViolation();
									return getCheckResults();
								}				
			   	.              	{}
		}

/************************/
/* CONSUMECOMMAND STATE	*/
/************************/
<CONSUMECOMMAND>   	
		{
				{PIPEIGNORE}	{pipeline = false; yybegin(YYINITIAL);}
				{PIPE}			{pipeline = true; yybegin(PIPELINE);}
				\n | ";"		{yybegin(YYINITIAL);}
				<<EOF>>			{    
									linesError=0;
									if(!verified && functionCalled) addViolation();
									return getCheckResults();
								}
			   	.              	{}
		}

		
		
/************************/
/* CHECKRET STATE	    */
/************************/
<CHECKRET>   	
		{
				{COMMENT_WORD}	{yybegin(COMMENTARGS);}
				{VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		addViolation();
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
			    				 		functionCall=yytext();
			    				 		yybegin(FINLINE);
			    				} else {
			    					functionCalled=false;
			    				}} 
				\$\?			{verified=true;}
				{SPACE}			{}
				\n				{
								 if(!verified) addViolation();
								 functionCalled = false;
								 functionCall="";
								 yybegin(YYINITIAL);}
				<<EOF>>			{
									if(!verified && functionCalled) addViolation();
									return getCheckResults();
								}
			   	.              	{}
		}
		
/************************/
/* COMMENTARGS STATE	*/
/************************/
<COMMENTARGS>   	
		{
				\n				{linesError++; yybegin(CHECKRET);}
				<<EOF>>			{    
									linesError=0;
									if(!verified && functionCalled) addViolation();
									return getCheckResults();
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
				<<EOF>>			{return getCheckResults();}
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
				<<EOF>>				{return getCheckResults();}
		  	 	[^]|{SPACE}  		{}
		}

/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
				{IGNORE_STRING_D}	{}
				{STRING_D}    		{yybegin(YYINITIAL);}  
				<<EOF>>				{return getCheckResults();}
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