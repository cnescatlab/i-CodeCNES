/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the MAXImbric.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.icode.shell.metrics;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

import java.util.EmptyStackException;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import java.util.logging.Logger;



%%

%class SHMETLineOfComment
%extends AbstractChecker
%public
%column
%line


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
    private String parsedFileName;
	private List<String> identifiers = new LinkedList<String>();
	private float lines=0;
	private boolean emptyLine = true;
	private boolean inStringSimpleQuoted = false;
	private boolean inStringDoubleQuoted = false;
	private float lastLinesCommented = 0;
	private int functionLine;
	private float commentLinesMain=0;
	private float commentLinesTotal=0;
	private float linesMain=0;
	private float linesTotal=0;
	private Stack<FunctionLineOfComment> functionStack = new Stack<>();
	private static final Logger LOGGER = Logger.getLogger(SHMETLineOfComment.class.getName());	
	
	public SHMETLineOfComment(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		LOGGER.fine("begin method setInputFile");
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
		LOGGER.fine("end method setInputFile");
	}
	
	private void endLocation() throws JFlexException {
		LOGGER.fine("begin method endLocation");
		try{
		    FunctionLineOfComment functionFinished = functionStack.pop();
       		LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] computing function :"+functionFinished.getName()+" line :"+ functionFinished.getBeginLine()+" with value : "+(functionFinished.getLineOfComment()/functionFinished.getLineOfCode())*100);
 			this.computeMetric(functionFinished.getName(), functionFinished.getLineOfComment(), functionFinished.getBeginLine());
			
			if(functionStack.empty()){
				linesMain+=functionFinished.getLineOfCode();
				commentLinesMain+=functionFinished.getLineOfComment();
			}else{
				FunctionLineOfComment function = functionStack.peek();
				function.setLineOfCode(function.getLineOfCode()+functionFinished.getLineOfCode());
				function.setLineOfComment(function.getLineOfComment()+functionFinished.getLineOfComment());
			}
		}catch(EmptyStackException e){
		    
		    final String errorMessage = e.getMessage();
		    throw new JFlexException(this.getClass().getName(), parsedFileName,
		                    errorMessage, yytext(), yyline, yycolumn);
		}
		LOGGER.fine("end method setInputFile");
	}
	
	private void addLines(){
		LOGGER.fine("begin method addLines");
		if(!emptyLine){
			if(functionStack.empty()){
				LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line to MAIN PROGRAM");
				linesMain++;
			} else {
				LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line to the function "+ functionStack.peek().getName());
				functionStack.peek().addLineOfCode();
			}
			LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one line for the whole file");
			linesTotal++;
		}
		LOGGER.fine("end method addLines");
	}
	private void addCommentLines(){
		LOGGER.fine("begin method addCommentLines");
		if(functionStack.empty()){
			LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one comment line to MAIN PROGRAM");
			commentLinesMain++;
		} else {
			LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one comment line to the function "+ functionStack.peek().getName());
			functionStack.peek().addLineOfComment();
		}
		LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] counting one comment line for the whole file");
		commentLinesTotal++;
		LOGGER.fine("end method addLines");
	}
	
%}

%eofval{
	if(functionStack.empty()){
		this.computeMetric("MAIN PROGRAM", commentLinesMain, 1);
	}else{
		
		    final String errorMessage = "Analysis failure : At least one function is not ending correctly.";
		    throw new JFlexException(this.getClass().getName(), parsedFileName,
		                    errorMessage, yytext(), yyline, yycolumn);
	}
	this.computeMetric(null, commentLinesTotal, 0);
	
	return getCheckResults();
%eofval}
%%

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{	
									lastLinesCommented++;
									//Count pending the value of emptyline (as the comment might be on the right side of some code)
									addLines();
									addCommentLines();
									emptyLine=true;
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
				
				
				{COMMENT_WORD} 	{
		  							LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
		  							yybegin(COMMENT);
		  						}	
				{FUNCTION}     	{
									emptyLine = false;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
									yybegin(NAMING);
								}
				{FUNCT}			{
									emptyLine = false;
									functionLine = yyline+1;
									location = yytext().substring(0,yytext().length()-2).trim();
								 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BEGINFUNC (Transition : FUNCT \""+yytext()+"\" )");
								 	yybegin(BEGINFUNC);
							 	}
				
	      		{FUNCSTART}		{
	      							emptyLine = false;
	      							if(!functionStack.empty()){
	      								if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
	      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] addStarterRepetition() for FUNCSTART  \""+yytext()+"\" )");
	      									functionStack.peek().addStarterRepetition();
	      								}
	      								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for FUNCSTART  \""+yytext()+"\" )");
	      							}
	      						}
	      		{FUNCEND}		{
	      							lastLinesCommented=0;
	      							emptyLine = false;
	      							if(!functionStack.empty()){
	      								if(functionStack.peek().isFinisher(yytext())){
	      									if(functionStack.peek().getStarterRepetition()>0) {
	      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] removeStarterRepetition() for FUNCEND  \""+yytext()+"\" )");
	      										try{
	      										    functionStack.peek().removeStarterRepetition();
	      										}catch(JFlexException e){
	      										    
												    final String errorMessage = e.getMessage();
												    throw new JFlexException(this.getClass().getName(), parsedFileName,
												                    errorMessage, yytext(), yyline, yycolumn);
	      										}
	      									} else {
	      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] endLocation() for FUNCEND  \""+yytext()+"\" )");
	      										addLines();
	      										emptyLine=true;
	      										endLocation();
	      									}
	      								}
	      							}
	      						}
	      		{VAR}			{ 
									lastLinesCommented=0;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for VAR  \""+yytext()+"\" )");
									emptyLine = false;
								}
				{IGNORE}		{   
									lastLinesCommented=0;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for IGNORE  \""+yytext()+"\" )");
									emptyLine = false;
								}
				{COMMAND}		{
									lastLinesCommented=0;
									emptyLine = false;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMAND (Transition : COMMAND \""+yytext()+"\" )");
								 	yybegin(COMMAND);
								}
				{STRING_S}		{  
									lastLinesCommented=0;
									emptyLine = false; 
									inStringSimpleQuoted = true;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : STRING_S \""+yytext()+"\" )");
								 	yybegin(STRING);
								}
				{STRING_D}		{
									lastLinesCommented=0;
									emptyLine = false; 
									inStringDoubleQuoted = true;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : STRING_D \""+yytext()+"\" )");
								 	yybegin(STRING);
								}
				      					
	      		\n 				{
	      							lastLinesCommented=0;
	      							addLines();
	      							emptyLine=true;
	      						}
	      		{SPACE}			{ }
	      		. 				{
									lastLinesCommented=0;
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
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> BEGINFUNC (Transition : VAR \""+yytext()+"\" )");
									yybegin(BEGINFUNC);
								}
				\n             	{
									addLines();
									emptyLine = true;
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
									FunctionLineOfComment function = new FunctionLineOfComment(location, functionLine, yytext());
      								if(lastLinesCommented>0){
      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] Transfering "+ lastLinesCommented +" lines detected as header comment from the last function to the new one. )");
	      					 			if(functionStack.empty()){
	      									commentLinesMain-=lastLinesCommented;
	      								}else{
			                                functionStack.peek().setLineOfComment(functionStack.peek().getLineOfComment() - lastLinesCommented);
			                            }
		                            	function.setLineOfComment(lastLinesCommented);
		                            	lastLinesCommented = 0;
  									}
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] push("+location+") for FUNCSTART  \""+yytext()+"\" )");
									functionStack.push(function);
								 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> YYINITIAL (Transition : FUNCSTART \""+yytext()+"\" )");
								 	yybegin(YYINITIAL);
							 	}
			   	[^]|{SPACE} 	{}
		}
/************************/
/* COMMAND STATE	    */
/************************/
<COMMAND>
		{
				\n			{ 
								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] count line for \\n");
								addLines();
							}
				{COMMAND}	{
								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMAND -> YYINITIAL (Transition : COMMAND \""+yytext()+"\" )");
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
								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING] count line for \\n");
								addLines();
							}
				{IGNORE}	{
								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING] do nothing for IGNORE  \""+yytext()+"\" )");
							}
				{STRING_S}	{
								if(inStringSimpleQuoted){
									inStringSimpleQuoted=false;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING_S \""+yytext()+"\" )");
									yybegin(YYINITIAL);
								}
								
							}
				{STRING_D}	{
								if(inStringDoubleQuoted){
									inStringDoubleQuoted=false;
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING_D \""+yytext()+"\" )");
									yybegin(YYINITIAL);
								}
								
							}
				. | {SPACE} { }
		}
