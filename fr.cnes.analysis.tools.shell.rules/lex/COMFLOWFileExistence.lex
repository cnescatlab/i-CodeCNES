/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.FileExistence rule.  */
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

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

import java.util.logging.Logger;

%%

%class COMFLOWFileExistence
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, FILE, STRING

COMMENT_WORD = [\#]
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+\^\*]+
SPACE		 = [\ \r\t\f]
NAME	     = ([a-zA-Z\_][a-zA-Z0-9\_]*)
SHELL_VAR	 = ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR = [\!]?{NAME}([\:]|(([\%]?[\%])|([\#]?[\#]))|([\:]?[\=\+\?\-]))({NAME}|[\[]{NAME}[\]])|([\#]{NAME})
VAR 		 = ({NAME}|([\$][\{]({NAME}|{SHELL_VAR}|{EXPANDED_VAR})[\}])|([\$]({NAME}|{SHELL_VAR}))) 


OPERATOR_RIGHT  = [\>]|[\>][\&]|[\&][\>]|[\>][\>]|[\>][\>][\>]
OPERATOR_LEFT	= [\<]|[\<][\&]|[\&][\<]|[\<][\<]|[\<][\<][\<]
OPERATOR_RL		= [\<][\>]
RIGHT_FILE_REDIRECT = ({OPERATOR_RIGHT}|{OPERATOR_RL}){SPACE}*{FILESTRING}
LEFT_FILE_REDIRECT	= {FILESTRING}{SPACE}*({OPERATOR_LEFT}|{OPERATOR_RL})
REDIRECT_IGNORE		= ([0-2]({OPERATOR_LEFT}|{OPERATOR_RL})) | (({OPERATOR_RIGHT}|{OPERATOR_RL})[0-2])


STRING		 = [\"]|[\']
ESCAPE		 = [\\]
FILE_SEPARATOR = [\/]|[\\]

FILECHAR	 =	[a-zA-Z0-9\_\.\?\!\^\+\*\-\%\§]
FILEWORD	 = (([\.]?{FILE_SEPARATOR})|([\~]))?(({FILECHAR}+|{VAR}){FILE_SEPARATOR}?)+
FILESTRING	= (([\"]{SPACE}*{FILEWORD}{SPACE}*[\"])|([\']{SPACE}*{FILEWORD}{SPACE}*[\'])|{FILEWORD})+

FILEEXIST	 = "test" (\!)? {SPACE}+ {OPTION}{SPACE}+{FILESTRING}+ | \[ {SPACE}* (\!)? {SPACE}+ {OPTION}{SPACE}+{FILESTRING}+ {SPACE}+ \]

OPTION	 = \- ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "k" |
				   "p" | "r" | "s" | "u" | "w" | "x" | "O" | "G" | "L" |
				   "N" | "S" | "z" | "n")


COMMAND_END  = [\n] | [\;] | [\|] | [\`]		
COMMAND_NAME = "cat" | "tee" | "more" | "less" | "head" | "wc" | "sh" | "rm" 
GREP_COMMAND = "grep"{SPACE}+(([\-][AB]{SPACE}+[0-9]+{SPACE}+)?|([\-]?[\-][a-zC-Z]+{SPACE}+)?)*([\-][\e]{SPACE}+)?[\^]?(([\"]([^\"]|([\\][\"]))*[\"])|([\']([^\']|([\\][\']))*[\'])|({FNAME}+|{VAR})+)
BASE_COMMAND = {COMMAND_NAME}{SPACE}+([\-]?[\-]({VAR}|{FNAME})+([\=]({VAR}|{FNAME})+)?{SPACE}*)*{SPACE}*
FILE_COMMAND = {GREP_COMMAND} | {BASE_COMMAND}



STRING_ESCAPED = [\\]{STRING}
IGNORE		   = {REDIRECT_IGNORE} | {STRING_ESCAPED} | ([\\][\#]) | "ssh"

																
%{
    private static final Logger LOGGER = Logger.getLogger(COMFLOWFileExistence.class.getName());
	private String location = "MAIN PROGRAM";
    private String parsedFileName;
    
    
	List<String> filesExistence = new ArrayList<String>();
	
	private String stringBeginner = ""; 
	private boolean	escapeNext = false;
	private boolean ignoring   = false;

    public COMFLOWFileExistence() {
		filesExistence.add("/dev/null");
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.fine("begin method setInputFile");
        
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.fine("end method setInputFile");
	}
		
%}

%eofval{
	return getCheckResults();
%eofval}


%%          




/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); 
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : VAR \""+yytext()+"\" )");
									yybegin(YYINITIAL);}
				\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	
				{FUNCTION}   	  							{	
																LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : FUNCTION \""+yytext()+"\" )");
																yybegin(NAMING);
															}
				{FUNCT}										{	
																location = yytext().substring(0,yytext().length()-2).trim();
															}
			    {FILEEXIST}									{
			    												int index = yytext().indexOf('-');
															 	String subfile = yytext().substring(index);
															 	String file = subfile.replaceAll("\"", "").replaceAll("\\{", "").replaceAll("\\}", "").replaceAll("]", "").split(" ")[1];
			                     							 	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - File test detected on FILEEXIST \""+file+"\" .");
			                     							 	filesExistence.add(file);
		                     							 	}
			                     							 	
				
			    
			    {IGNORE}									{
		 														LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IGNORE \""+yytext()+"\" .");
			    											}
			    {LEFT_FILE_REDIRECT}|{RIGHT_FILE_REDIRECT}	{	
								    							
								    							String name = yytext().replaceAll("([\\s]|[\\>]|[\\\"]|[\\']|[\\<]|[\\&]|[\\{]|[\\}])+","");
								    							LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - Redirection to file "+name+" detected.");
								    							if(!filesExistence.contains(name)){
								    								LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError on "+name+" file redirection.");
								    								setError(location,"The existence of the file " + name + " has not been checked.", yyline+1);
								    							}
								    						}
			 	{COMMENT_WORD} 								{
			  													if(!escapeNext){
			  														LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			  														yybegin(COMMENT);
			  													}
		  													}	
			 	{FILE_COMMAND}								{	
																LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> FILE (Transition : FILE_COMMAND \""+yytext()+"\" )");
																yybegin(FILE);
															}
				{STRING}									{
		 														stringBeginner=yytext();
		 														LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : STRING \""+yytext()+"\" )");
		 														yybegin(STRING);
			 												}									
			 	[^]            								{}
			 	
		}	
		
/************************/
/*  STRING STATE      	*/
/************************/
<STRING>
		{
				{ESCAPE}									{
																if(!escapeNext){
																	escapeNext=true;
																 }else{
																 	escapeNext=false;
																 }
															}
															
				{STRING}								{
																if(!escapeNext && yytext().equals(stringBeginner)){
																	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING \""+yytext()+"\" )");
																	yybegin(YYINITIAL);
																}
																escapeNext=false;
															}
				{COMMENT_WORD}								{escapeNext=false;}
				[^]          								{escapeNext=false;}											
				
								
		}		
/************************/
/*  FILE STATE      	*/
/************************/
<FILE>
		{
				{FILESTRING}+	    							{
							    								String name = yytext().replaceAll("([\\\"]|[\\']|[\\{]|[\\}]|[\\[]|[\\]])+","");
							    							 	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - Command using file \""+name+"\" detected.");
							    							 	if(!filesExistence.contains(name)){
							    							 		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError on file \""+yytext()+"\" on command call.");
							    							 		setError(location,"The existence of the file " + name + " has not been checked.", yyline+1);
							    							 	}
							    							 	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FILE -> YYINITIAL (Transition : FILESTRING \""+yytext()+"\" )");
							    								escapeNext=false;
							    								yybegin(YYINITIAL);
							    							}
				{COMMAND_END}								{
																LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FILE -> YYINITIAL (Transition : \""+yytext()+"\" )");
																escapeNext=false;
																yybegin(YYINITIAL);}
				{LEFT_FILE_REDIRECT}|{RIGHT_FILE_REDIRECT}	{	
								    							String name = yytext().replaceAll("([\\s]|[\\>]|[\\\"]|[\\']|[\\<]|[\\&]|[\\{]|[\\}])+","");
								    							LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - Redirection to file "+name+" detected.");
								    							if(!filesExistence.contains(name)){
								    								LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError on "+name+" file redirection.");
								    								setError(location,"The existence of the file " + name + " has not been checked.", yyline+1);
								    							}
								    							LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FILE -> YYINITIAL (Transition : REDIRECT \""+yytext()+"\" )");
								    							escapeNext=false;
								    							yybegin(YYINITIAL);
								    						}
				.				{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}