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
import java.util.List;
import java.util.Stack;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

%%

%class SHMETComplexitySimplified
%extends AbstractChecker
%public
%ignorecase
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, BEGINFUNC, STRING_DOUBLE, STRING_SIMPLE, COMMAND, CASE


COMMENT_WORD 	= [\#]
FUNCT			= {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION    	= "function"
FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE			= [\ \r\t\f\space]
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR		= ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR	= [\$][\{](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\:\%\=\+\?\/\!\-\,\^\#\*\@]|([\[](([\:]{SPACE}*[\-])|[a-zA-Z0-9\_\/\:\%\=\+\?\!\$\-\,\^\#\*\@\[\]\{\}])+[\]]))+[\}]
VAR				= {NAME}|{EXPANDED_VAR}|([\$]({NAME}|{SHELL_VAR}))
IGNORE_COMMAND  = [\\][\`]
COMMAND			= [\`]|([\$][\(])
END_COMMAND		= [\`]|[\)]
STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']
IGNORE			= {IGNORE_STRING_D} | {IGNORE_STRING_S} | {IGNORE_COMMAND}
FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "done"
COMPLEX			= "else"	| "elif"
CASE			= "case"
ESAC 			= "esac"
CASE_STATEMENT	=  ({SPACE}*([^\space\(\)\n]*|{VAR})+{SPACE}*)([\|]({SPACE}*([^\space\(\)\n]*|{VAR})+{SPACE}*))*[^\(\)][\)]
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	private Stack<FunctionComplexitySimplified> functionStack = new Stack<>();
	float mainComplexity = 1;
	float totalComplexity = 0;
	int caseState = 0;
	int functionLine = 0;
	Stack<String> commandClosureStack = new Stack<>();
	private static final Logger LOGGER = Logger.getLogger(SHMETComplexitySimplified.class.getName());	
	
	
	
	public SHMETComplexitySimplified(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	private void endLocation() throws JFlexException {
		LOGGER.fine("begin method endLocation");
		try{
		    FunctionComplexitySimplified functionFinished = functionStack.pop();
	       	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] computing function :"+functionFinished.getName()+" line :"+ functionFinished.getBeginLine()+" with value : "+functionFinished.getComplexity());
      	 	totalComplexity+=functionFinished.getComplexity();
      	 	this.computeMetric(functionFinished.getName(), functionFinished.getComplexity(), functionFinished.getBeginLine());
		}catch(EmptyStackException e){
		    String errorMessage = "Class"+this.getClass().getName()+"\n"+e.getMessage()+"\nFile :"+ this.getInputFile().getAbsolutePath() + "\nat line:"+yyline+" column:"+yycolumn;
		    throw new JFlexException(new Exception(errorMessage));
		}
		LOGGER.fine("end method endLocation");
	}
	
%}

%eofval{
	this.computeMetric("MAIN PROGRAM", mainComplexity, 0);
	this.computeMetric(null, mainComplexity+totalComplexity, 0);
	return getCheckResults();
%eofval}
%%

/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				 \n			 	{	
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);
								}  
				. | {SPACE} 	{ 
									LOGGER.fine("Do nothing");
								}
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
		      								}
	      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] computing complexity for FUNCSTART  \""+yytext()+"\" )");
	      									functionStack.peek().addComplexity(FunctionComplexitySimplified.computeComplexity(yytext()));
		      							} else {
		      								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] computing complexity for main function with FUNCSTART  \""+yytext()+"\" )");
		      								mainComplexity += FunctionComplexitySimplified.computeComplexity(yytext());
		      							}
		      							
		      						}
		      						
		      	{COMPLEX}			{
										if(functionStack.empty()){
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] computing complexity of OTHER_COMPLEX  \""+yytext()+"\" ) for the main function");
											mainComplexity += FunctionComplexitySimplified.computeComplexity(yytext());
										}else{
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] computing complexity of OTHER_COMPLEX  \""+yytext()+"\" ) for the function "+functionStack.peek().getName()+".");
											functionStack.peek().addComplexity(FunctionComplexitySimplified.computeComplexity(yytext()));
										}
									}
				{CASE}				{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] increasing case number to "+caseState+" for CASE  \""+yytext()+"\" .");
										caseState++;
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STATE (Transition : CASE \""+yytext()+"\" )");
										yybegin(CASE);
									}

				
	      		{FUNCEND}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().isFinisher(yytext())){
		      									if(functionStack.peek().getStarterRepetition()>0) {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] removeStarterRepetition() for FUNCEND  \""+yytext()+"\" )");
	      										    functionStack.peek().removeStarterRepetition();
		      									} else {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] endLocation() for FUNCEND  \""+yytext()+"\" )");
		      										endLocation();
		      									}
		      								}else{
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] Do nothing for FUNCEND  \""+yytext()+"\" )");
		      								}
		      							}else{
			      							LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] Do nothing for FUNCEND  \""+yytext()+"\" )");
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
										commandClosureStack.push(ShellUtils.commandClosure(yytext()));
										yybegin(COMMAND);
									}
				{VAR}				{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] do nothing for IGNORE  \""+yytext()+"\" )");
									}

	      		[^]|{SPACE}  		{
										LOGGER.fine("Do nothing");
									}
		}
/************************/
/* CASE STATE	     	*/
/************************/		
<CASE>
		{
				
				{COMMENT_WORD}	 	{
			  							LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			  							yybegin(COMMENT);
			  						}
			  						
				{FUNCTION}     		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
										yybegin(NAMING);
									}
				
				{FUNCT}				{
										functionLine = yyline+1;
										location = yytext().substring(0,yytext().length()-2).trim();
									 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> BEGINFUNC (Transition : FUNCT \""+yytext()+"\" )");
									 	yybegin(BEGINFUNC);
								 	}
								 	
	      		{FUNCSTART}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] addStarterRepetition() for FUNCSTART  \""+yytext()+"\" )");
		      									functionStack.peek().addStarterRepetition();
		      								}
	      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] computing complexity for FUNCSTART  \""+yytext()+"\" )");
	      									functionStack.peek().addComplexity(FunctionComplexitySimplified.computeComplexity(yytext()));
		      							} else {
		      								LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] computing complexity for main function with FUNCSTART  \""+yytext()+"\" )");
		      								mainComplexity += FunctionComplexitySimplified.computeComplexity(yytext());
		      							}
		      							
		      						}
		      						
		      	{COMPLEX}			{
										if(functionStack.empty()){
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] computing complexity of OTHER_COMPLEX  \""+yytext()+"\" ) for the main function");
											mainComplexity += FunctionComplexitySimplified.computeComplexity(yytext());
										}else{
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] computing complexity of OTHER_COMPLEX  \""+yytext()+"\" ) for the function "+functionStack.peek().getName()+".");
											functionStack.peek().addComplexity(FunctionComplexitySimplified.computeComplexity(yytext()));
										}
									}
				
				{CASE}				{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] increasing case number to "+caseState+" for CASE  \""+yytext()+"\" .");
										caseState++;
									}
				{ESAC}				{
										if(caseState > 0){
											caseState--;
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] decreasing case number to "+caseState+" for CASE  \""+yytext()+"\" .");
											
											if(!functionStack.empty()){
			      								if(functionStack.peek().isFinisher(yytext())){
			      									if(functionStack.peek().getStarterRepetition()>0) {
			      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] removeStarterRepetition() for ESAC  \""+yytext()+"\" )");
		      										    functionStack.peek().removeStarterRepetition();
			      									} else {
			      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Running endLocation() for ESAC  \""+yytext()+"\" )");
			      										endLocation();
			      									}
			      								}else{
			      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Case ending is not closing a function for ESAC : \""+yytext()+"\" ");
		      									}
		      								}
		      								if(caseState==0){
		      									yybegin(YYINITIAL);
		      								}
										}else{
											String errorMessage = "Class: "+this.getClass().getName()+"\nImpossible to handle case closure ESAC because no case has been declared.\nFile :"+ this.getInputFile().getAbsolutePath() +"\nat line:"+yyline+" column:"+yycolumn+"\nLast word read : "+yytext();
											throw new JFlexException(new Exception(errorMessage));	
										}
									}
				{CASE_STATEMENT}	{
										if(caseState>0){
											if(functionStack.empty()){
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Compute case statement for main function with CASE_STATEMENT \""+yytext()+"\".");
												mainComplexity++;
											}else{
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Compute case statement for "+functionStack.peek().getName()+" for CASE_STATEMENT \""+yytext()+"\".");
												functionStack.peek().computeCase();
											}	
										}else{
											String errorMessage = "Class: "+this.getClass().getName()+"\nHandling CASE_STATEMENT while no case were declared.\nFile :"+ this.getInputFile().getAbsolutePath() +"\nat line:"+yyline+" column:"+yycolumn+"\nLast word read : "+yytext();
											throw new JFlexException(new Exception(errorMessage));	
										}										
									}
				{FUNCEND}			{
		      							if(!functionStack.empty()){
		      								if(functionStack.peek().isFinisher(yytext())){
		      									if(functionStack.peek().getStarterRepetition()>0) {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] removeStarterRepetition() for FUNCEND  \""+yytext()+"\" )");
	      										    functionStack.peek().removeStarterRepetition();
		      									} else {
		      										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] endLocation() for FUNCEND  \""+yytext()+"\" )");
		      										endLocation();
		      									}
		      								}else{
		      									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Do nothing for FUNCEND  \""+yytext()+"\" )");
		      								}
		      							}else{
			      							LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] Do nothing for FUNCEND  \""+yytext()+"\" )");
  										}
		      						}
		      	{IGNORE}			{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] do nothing for IGNORE  \""+yytext()+"\" )");
									}
				{STRING_D}			{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> STRING_DOUBLE (Transition : STRING_D \""+yytext()+"\" )");
										yybegin(STRING_DOUBLE);
									}
				{STRING_S}			{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> STRING_SIMPLE (Transition : STRING_S \""+yytext()+"\" )");
										yybegin(STRING_SIMPLE);
									}
				{COMMAND}			{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - CASE -> COMMAND (Transition : COMMAND \""+yytext()+"\" )");
										commandClosureStack.push(ShellUtils.commandClosure(yytext()));
										yybegin(COMMAND);
									}
				
		      	
		      	{VAR}				{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [CASE] do nothing for IGNORE  \""+yytext()+"\" )");
									}


				      					
	      		[^]|{SPACE}  		{
										LOGGER.fine("Do nothing");
									}
		}
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
					{IGNORE}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [STRING_SIMPLE] do nothing for IGNORE  \""+yytext()+"\" )");
									}
					{STRING_S}    	{
										if(commandClosureStack.empty()){
											if(caseState==0){
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_SIMPLE -> YYINITIAL (Transition STRING_S : \""+yytext()+"\" )");
												yybegin(YYINITIAL);
											}else{
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_SIMPLE -> CASE (Transition STRING_S : \""+yytext()+"\" )");
												yybegin(CASE);
											}
										}else{
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_SIMPLE -> COMMAND (Transition STRING_S : \""+yytext()+"\" )");
											yybegin(COMMAND);
										}
									}  
		  	 	[^]|{SPACE}  		{
										LOGGER.fine("Do nothing");
									}
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
										if(commandClosureStack.empty()){
											if(caseState==0){
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_DOUBLE -> YYINITIAL (Transition STRING_D : \""+yytext()+"\" )");
												yybegin(YYINITIAL);
											}else{
												LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_DOUBLE -> CASE (Transition STRING_D : \""+yytext()+"\" )");
												yybegin(CASE);
											}
										}else{
											LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - STRING_DOUBLE -> COMMAND (Transition STRING_D : \""+yytext()+"\" )");
											yybegin(COMMAND);
										}
									}  
		  	 	[^]|{SPACE}  		{
										LOGGER.fine("Do nothing");
		  	 						}
		}
/************************/
/* COMMAND STATE	    */
/************************/
<COMMAND>   	
		{
					{IGNORE}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] do nothing for IGNORE  \""+yytext()+"\" )");
	
									}
					{STRING_D}		{   
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING_DOUBLE (Transition : STRING_D \""+yytext()+"\" )");
										yybegin(STRING_DOUBLE);
									}
					{STRING_S}		{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING_SIMPLE (Transition : STRING_S \""+yytext()+"\" )");
										yybegin(STRING_SIMPLE);
									}
					{FUNCT}			{}
					{FUNCSTART}	{}
					{VAR}			{}
					{END_COMMAND}  	{
										if(commandClosureStack.empty()){
											throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Command closure unreachable.", yytext(), yyline, yycolumn);	
										}
										if(yytext().equals(commandClosureStack.peek())){
											
											commandClosureStack.pop();
											if(commandClosureStack.empty()){
												if(caseState==0){
													LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMAND -> YYINITIAL (Transition COMMAND : \""+yytext()+"\" )");
													yybegin(YYINITIAL);
												}else{
													LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - COMMAND -> CASE (Transition COMMAND : \""+yytext()+"\" )");
													yybegin(CASE);
												}
											}
										}else{
											//Do nothing
										}
									}  
		  	 	[^]|{SPACE}  		{
		  	 							LOGGER.fine("Do nothing");
		  	 						}
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
									if(caseState==0){
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
										yybegin(YYINITIAL);
									}else{
										LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> CASE (Transition : \\n )");
										yybegin(CASE);
									}
								}  
			   	. | {SPACE}     {
			   						LOGGER.fine("Do nothing");
			   					 }
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
				\(\)			{
									LOGGER.fine("Do nothing");
								}
				{FUNCSTART}		{
									FunctionComplexitySimplified function;
									function = new FunctionComplexitySimplified(location, functionLine, yytext());
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] push("+location+") for FUNCSTART  \""+yytext()+"\" )");
									functionStack.push(function);
								 	if(caseState==0){
								 		LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> YYINITIAL (Transition : FUNCSTART \""+yytext()+"\" )");
								 		yybegin(YYINITIAL);
								 	}else{
								 		LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> CASE (Transition : FUNCSTART \""+yytext()+"\" )");
								 		yybegin(CASE);
								 	}
							 	}
				{CASE}			{
									caseState++;
									FunctionComplexitySimplified function;
									function = new FunctionComplexitySimplified(location, functionLine, yytext());
									LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - [BEGINFUNC] push("+location+") for CASE  \""+yytext()+"\" )");
									functionStack.push(function);
								 	LOGGER.fine("["+ this.getInputFile().getAbsolutePath()+":"+(yyline+1)+":"+yycolumn+"] - BEGINFUNC -> CASE (Transition : CASE \""+yytext()+"\" )");
									yybegin(CASE);
								}
			   	[^]|{SPACE}  {	
			   						LOGGER.fine("Do nothing");
			   					}
		}
