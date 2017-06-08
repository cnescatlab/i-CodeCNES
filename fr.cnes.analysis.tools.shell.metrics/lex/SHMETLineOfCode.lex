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

%class SHMETLineOfCode
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, BEGINFUNC, STRING, COMMAND

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
	private List<String> identifiers = new LinkedList<String>();
	private float lines=0;
	private boolean emptyLine = true;
	private boolean inStringSimpleQuoted = false;
	private boolean inStringDoubleQuoted = false;
	private int functionLine;
	private float linesMain=0;
	private float linesTotal=0;
	private Stack<FunctionLineOfCode> functionStack = new Stack<>();
	private static final Logger LOGGER = Logger.getLogger(SHMETLineOfCode.class.getName());	
	
	public SHMETLineOfCode(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		LOGGER.info("begin method setInputFile");
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
		LOGGER.info("end method setInputFile");
	}
	
	private void endLocation() throws JFlexException {
		LOGGER.info("begin method endLocation");
		try{
		    FunctionLineOfCode functionEnded = (FunctionLineOfCode) functionStack.pop();
	       	LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] computing function :"+functionEnded.getName()+" line :"+ functionEnded.getBeginLine()+" with value : "+functionEnded.getLineOfCode());
      	 	this.computeMetric(functionEnded.getName(), functionEnded.getLineOfCode(), functionEnded.getBeginLine());
			if(functionStack.empty()){
				linesMain+=functionEnded.getLineOfCode();
			}else{
				FunctionLineOfCode function = (FunctionLineOfCode) functionStack.peek();
				function.setLineOfCode(function.getLineOfCode()+functionEnded.getLineOfCode());
			}
		}catch(EmptyStackException e){
		    String errorMessage = "Class"+this.getClass().getName()+"\n"+e.getMessage()+"\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		    throw new JFlexException(new Exception(errorMessage));
		}
		LOGGER.info("end method setInputFile");
	}
	
	private void addLines(){
		LOGGER.info("begin method addLines");
		if(!emptyLine){
			if(functionStack.empty()){
				LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line to MAIN PROGRAM");
				linesMain++;
			} else {
				LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line to the function "+((FunctionLineOfCode) functionStack.peek()).getName());
				((FunctionLineOfCode) functionStack.peek()).addLineOfCode();
			}
			LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line for the whole file");
			linesTotal++;
		}
		LOGGER.info("end method addLines");
	}
	
%}

%eofval{
	if(functionStack.empty()){
		this.computeMetric("MAIN PROGRAM", linesMain, 1);
	}else{
		String errorMessage = "Class"+this.getClass().getName()+"\nunreadable by analyzer, at least one function is not ending correctly.\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		throw new JFlexException(new Exception(errorMessage));
	}
	this.computeMetric(null, linesTotal, 0);
	return getCheckResults();
%eofval}
%%

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{	
									//Count pending the value of emptyline (as the comment might be on the right side of some code)
									addLines();
									emptyLine=true;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);
								}  
				. | {SPACE} 	{ }
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				
				
				{COMMENT_WORD} 	{
		  							LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
		  							yybegin(COMMENT);
		  						}	
				{FUNCTION}     	{
									emptyLine = false;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
									yybegin(NAMING);
								}
				{FUNCT}			{
									emptyLine = false;
									functionLine = yyline+1;
									location = yytext().substring(0,yytext().length()-2).trim();
								 	LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BEGINFUNC (Transition : FUNCT \""+yytext()+"\" )");
								 	yybegin(BEGINFUNC);
							 	}
				
	      		{FUNCSTART}		{
	      							emptyLine = false;
	      							if(!functionStack.empty()){
	      								if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
	      									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] addStarterRepetition() for FUNCSTART  \""+yytext()+"\" )");
	      									functionStack.peek().addStarterRepetition();
	      								}
	      								LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for FUNCSTART  \""+yytext()+"\" )");
	      							}
	      						}
	      		{FUNCEND}		{
	      							emptyLine = false;
	      							if(!functionStack.empty()){
	      								if(functionStack.peek().isFinisher(yytext())){
	      									if(functionStack.peek().getStarterRepetition()>0) {
	      										LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] removeStarterRepetition() for FUNCEND  \""+yytext()+"\" )");
	      										try{
	      										    functionStack.peek().removeStarterRepetition();
	      										}catch(JFlexException e){
	      										    String errorMessage = "Class"+this.getClass().getName()+"\n"+e.getMessage()+"\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
	      										    throw new JFlexException(new Exception(errorMessage));
	      										}
	      									} else {
	      										LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] endLocation() for FUNCEND  \""+yytext()+"\" )");
	      										addLines();
	      										emptyLine=true;
	      										endLocation();
	      									}
	      								}
	      							}
	      						}
	      		{VAR}			{ 
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for VAR  \""+yytext()+"\" )");
									emptyLine = false;
								}
				{IGNORE}		{   
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for IGNORE  \""+yytext()+"\" )");
									emptyLine = false;
								}
				{COMMAND}		{
									emptyLine = false;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMAND (Transition : COMMAND \""+yytext()+"\" )");
								 	yybegin(COMMAND);
								}
				{STRING_S}		{  
									emptyLine = false; 
									inStringSimpleQuoted = true;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : STRING_S \""+yytext()+"\" )");
								 	yybegin(STRING);
								}
				{STRING_D}		{
									emptyLine = false; 
									inStringDoubleQuoted = true;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : STRING_D \""+yytext()+"\" )");
								 	yybegin(STRING);
								}
				      					
	      		\n 				{
	      							addLines();
	      							emptyLine=true;
	      						}
	      		{SPACE}			{ }
	      		. 				{
									emptyLine = false;
								}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{
	      							emptyLine = false;
									location = yytext();
									functionLine = yyline+1;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> BEGINFUNC (Transition : VAR \""+yytext()+"\" )");
									yybegin(BEGINFUNC);
								}
				\n             	{
									addLines();
									emptyLine = true;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
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
									FunctionLineOfCode function = new FunctionLineOfCode(location, functionLine, yytext());
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] push("+location+") for FUNCSTART  \""+yytext()+"\" )");
									functionStack.push(function);
								 	LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> YYINITIAL (Transition : FUNCSTART \""+yytext()+"\" )");
								 	yybegin(YYINITIAL);
							 	}
			   	. | \n |{SPACE} { }
		}
/************************/
/* COMMAND STATE	    */
/************************/
<COMMAND>
		{
				\n			{ 
								LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] count line for \\n");
								addLines();
							}
				{COMMAND}	{
								LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMAND -> YYINITIAL (Transition : COMMAND \""+yytext()+"\" )");
								yybegin(YYINITIAL);
							}
				. | {SPACE} { }
		}
/************************/
/* STRING STATE	    */
/************************/
<STRING>
		{
				\n			{ 
								LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING] count line for \\n");
								addLines();
							}
				{IGNORE}	{
								LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING] do nothing for IGNORE  \""+yytext()+"\" )");
							}
				{STRING_S}	{
								if(inStringSimpleQuoted){
									inStringSimpleQuoted=false;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING_S \""+yytext()+"\" )");
									yybegin(YYINITIAL);
								}
								
							}
				{STRING_D}	{
								if(inStringDoubleQuoted){
									inStringDoubleQuoted=false;
									LOGGER.info("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING_D \""+yytext()+"\" )");
									yybegin(YYINITIAL);
								}
								
							}
				. | {SPACE} { }
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]             {
									String errorMessage = "Class: "+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.getInputFile().getAbsolutePath() +"\nat line:"+yyline+" column:"+yycolumn+"\nLast word read : "+yytext();
                                    throw new JFlexException(new Exception(errorMessage));	
								}
				
