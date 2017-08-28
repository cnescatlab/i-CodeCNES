/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the MAXImbric.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.shell.metrics;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

import java.util.EmptyStackException;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

%%

%class SHMETNesting
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, BEGINFUNC, STRING_DOUBLE, STRING_SIMPLE, COMMAND

COMMENT_WORD 	= [\#]
FUNCT			= {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION    	= "function"
FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE			= [\ \r\t\f\space]
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR		= ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR	= [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR				= {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))
IGNORE_COMMAND  = \\`
COMMAND			= \`
STRING_D		= \"
IGNORE_STRING_D = \\\"
STRING_S	 	= \'
IGNORE_STRING_S = \\\'
IGNORE			= {IGNORE_STRING_D} | {IGNORE_STRING_S} | {IGNORE_COMMAND}
FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

%{
	private String location = "MAIN PROGRAM";
    private String parsedFileName;
	private List<String> identifiers = new LinkedList<String>();
	private Stack<FunctionNesting> functionStack = new Stack<>();
	private Stack<String> mainNestingStack = new Stack<>();
	private int mainNesting = 0;
	private int mainMaxNesting = 0;
	private int functionLine = 0;
	private static final Logger LOGGER = Logger.getLogger(SHMETNesting.class.getName());	
	
	public SHMETNesting(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		LOGGER.fine("begin method setInputFile");
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
		LOGGER.fine("end method setInputFile");
	}
	
	private void endLocation() throws JFlexException {
		LOGGER.fine("begin method endLocation");
		try{
		    FunctionNesting functionFinished = functionStack.pop();
	       	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] computing function :"+functionFinished.getName()+" line :"+ functionFinished.getBeginLine()+" with value : "+functionFinished.getMaxNesting());
      	 	this.computeMetric(functionFinished.getName(), functionFinished.getMaxNesting(), functionFinished.getBeginLine());
		}catch(EmptyStackException e){
		    String errorMessage = "Class"+this.getClass().getName()+"\n"+e.getMessage()+"\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		    throw new JFlexException(new Exception(errorMessage));
		}
		LOGGER.fine("end method endLocation");
	}

%}

%eofval{
	if(this.functionStack.empty()){
		this.computeMetric("MAIN PROGRAM", mainMaxNesting, 1);
	}else{
		String errorMessage = "Class"+this.getClass().getName()+"\nunreadable by analyzer, at least one function is not ending correctly.\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		throw new JFlexException(new Exception(errorMessage));
	}
	this.computeMetric(null, mainMaxNesting, 0);
	return getCheckResults();
%eofval}
%%

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{	
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);
								}  
				. | {SPACE} 	{ }
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				{COMMENT_WORD}	 	{
			  							LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			  							yybegin(COMMENT);
			  						}
				{FUNCTION}     		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
										yybegin(NAMING);
									}
				
				{FUNCT}				{
										functionLine = yyline+1;
										location = yytext().substring(0,yytext().length()-2).trim();
									 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BEGINFUNC (Transition : FUNCT \""+yytext()+"\" )");
									 	yybegin(BEGINFUNC);
								 	}
					
	      		{FUNCSTART}			{
		      							
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] addStarterRepetition() for FUNCSTART  \""+yytext()+"\" )");
		      									functionStack.peek().addStarterRepetition();
		      								}else{
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] No function change for FUNCSTART  \""+yytext()+"\" )");
	      									}
		      								if(FunctionNesting.isNesting(yytext())){
	      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] pushNesting() for FUNCSTART  \""+yytext()+"\" )");
	      										this.functionStack.peek().pushNesting(yytext());
	      									}
		      							}else{
		      								if(FunctionNesting.isNesting(yytext())){
	      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] Handle new nesting in the main for FUNCSTART  \""+yytext()+"\" )");
	      										this.mainNestingStack.push(yytext());
	      										this.mainNesting++;
	      										if(this.mainMaxNesting < mainNesting){
	      											this.mainMaxNesting = mainNesting;
	      										}
	      									}else{
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for FUNCSTART  \""+yytext()+"\" )");
	      									}
		      							}
		      							
		      						}
	      		{FUNCEND}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().isFinisher(yytext())){
		      									if(functionStack.peek().getStarterRepetition()>0) {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] removeStarterRepetition() for FUNCEND  \""+yytext()+"\" )");
		      										try{
			      										if(!functionStack.peek().getNesting().empty() && functionStack.peek().getNestingFinisher().equals(yytext())){
			      											functionStack.peek().popNesting();
			      										}
		      										
		      										    functionStack.peek().removeStarterRepetition();
		      										}catch(JFlexException e){
		      										    String errorMessage = "Class"+this.getClass().getName()+"\n"+e.getMessage()+"\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		      										    throw new JFlexException(new Exception(errorMessage));
		      										}
		      									} else {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] endLocation() for FUNCEND  \""+yytext()+"\" )");
		      										endLocation();
		      									}
		      								}else{
	      										if(!functionStack.peek().getNesting().empty() && functionStack.peek().getNestingFinisher().equals(yytext())){
		      											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] popNesting() for FUNCEND  \""+yytext()+"\" )");
		      											functionStack.peek().popNesting();
	      										}else{
	      											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] Doing nothing on imbrication for FUNCEND  \""+yytext()+"\" )");
      											}
		      								}
		      								
		      							}else{
			      							if(!this.mainNestingStack.empty() && FunctionNesting.nestingFinisherOf(this.mainNestingStack.peek()).equals(yytext())){
      											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] Handle nesting closing in the main for FUNCEND  \""+yytext()+"\" )");
      											this.mainNestingStack.pop();
      											this.mainNesting--;
      										}
  										}
		      						}
		      	{IGNORE}			{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for IGNORE  \""+yytext()+"\" )");
									}
				{STRING_D}			{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING_DOUBLE (Transition : STRING_D \""+yytext()+"\" )");
										yybegin(STRING_DOUBLE);
									}
				{STRING_S}			{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING_SIMPLE (Transition : STRING_S \""+yytext()+"\" )");
										yybegin(STRING_SIMPLE);
									}
				{COMMAND}			{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMAND (Transition : COMMAND \""+yytext()+"\" )");
										yybegin(COMMAND);
									}

		      	{VAR}				{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for IGNORE  \""+yytext()+"\" )");
									}

				      					
	      		\n | {SPACE} | . 	{}
		}
		
		
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
					{IGNORE}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING_S] do nothing for IGNORE  \""+yytext()+"\" )");
	
									}
					{STRING_S}    	{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_S -> YYINITIAL (Transition STRING_S : \""+yytext()+"\" )");
										yybegin(YYINITIAL);
									}  
		  	 	. | {SPACE} | \n	{}
		}
/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
					{IGNORE}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING_DOUBLE] do nothing for IGNORE  \""+yytext()+"\" )");
	
									}
					{STRING_D}    	{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_DOUBLE -> YYINITIAL (Transition STRING_D : \""+yytext()+"\" )");
										yybegin(YYINITIAL);
									}  
		  	 	. | {SPACE} | \n	{}
		}
/************************/
/* COMMAND STATE	    */
/************************/
<COMMAND>   	
		{
					{IGNORE}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] do nothing for IGNORE  \""+yytext()+"\" )");
	
									}
					{COMMAND}    	{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMAND -> YYINITIAL (Transition COMMAND : \""+yytext()+"\" )");
										yybegin(YYINITIAL);
									}  
		  	 	. | {SPACE} | \n	{}
		}
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{
									location = yytext();
									functionLine = yyline+1;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> BEGINFUNC (Transition : VAR \""+yytext()+"\" )");
									yybegin(BEGINFUNC);
								}
				\n             	{
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);
								}  
			   	. | {SPACE}     {}
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
									FunctionNesting function;
									if(this.functionStack.empty()){
										function = new FunctionNesting(location, functionLine, yytext(), 1, 1);
									}else{
										function = new FunctionNesting(location, functionLine, yytext(),  1,  1);
									}
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] push("+location+") for FUNCSTART  \""+yytext()+"\" )");
									functionStack.push(function);
								 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> YYINITIAL (Transition : FUNCSTART \""+yytext()+"\" )");
								 	yybegin(YYINITIAL);
							 	}
			   	. | \n |{SPACE} { }
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}