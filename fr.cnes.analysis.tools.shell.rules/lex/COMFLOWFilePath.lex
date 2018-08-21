/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.FilePath rule.  	  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWFilePath
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, FILE, STRING

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
NAME	     = ([a-zA-Z\_][a-zA-Z0-9\_]*)
SHELL_VAR	 = ([0-9]+|[\-\@\?\#\!\_\*\$])
EXPANDED_VAR = [\!]?{NAME}([\:]|(([\%]?[\%])|([\#]?[\#]))|([\:]?[\=\+\?\-]))({NAME}|[\[]{NAME}[\]])|([\#]{NAME})
VAR 		 = ({NAME}|([\$][\{]({NAME}|{SHELL_VAR}|{EXPANDED_VAR})[\}])|([\$]({NAME}|{SHELL_VAR}))) 

STRING		 = [\"]|[\']
ESCAPE		 = [\\]
FILE_SEPARATOR = [\/]|[\\]

FILECHAR	 =	[a-zA-Z0-9\_\.\?\!\^\+\*\-\%\ยง]
FILEWORD	 = (([\.]?{FILE_SEPARATOR})|([\~]))?(({FILECHAR}+|{VAR}){FILE_SEPARATOR}?)+
FILESTRING	= (([\"]{SPACE}*{FILEWORD}{SPACE}*[\"])|([\']{SPACE}*{FILEWORD}{SPACE}*[\'])|{FILEWORD})+

FILEEXIST	 = "test" (\!)? {SPACE}+ {OPTION}{SPACE}+{FILESTRING}+ | \[ {SPACE}* (\!)? {SPACE}+ {OPTION}{SPACE}+{FILESTRING}+ {SPACE}+ \]

OPTION	 = \- ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "k" |
				   "p" | "r" | "s" | "u" | "w" | "x" | "O" | "G" | "L" |
				   "N" | "S")

COMMAND_END  = [\n] | [\;] | [\|] | [\`]		
COMMAND_NAME = "cat" | "tee" | "more" | "less" | "head" | "wc" | "sh" | "rm"
GREP_COMMAND = "grep"{SPACE}+(([\-][AB]{SPACE}+[0-9]+{SPACE}+)?|([\-]?[\-][a-zC-Z]+{SPACE}+)?)*([\-][\e]{SPACE}+)?[\^]?(([\"]([^\"]|([\\][\"]))*[\"])|([\']([^\']|([\\][\']))*[\'])|({FNAME}+|{VAR})+)
BASE_COMMAND = {COMMAND_NAME}{SPACE}+([\-]?[\-]({VAR}|{FNAME})+([\=]({VAR}|{FNAME})+)?{SPACE}*)*{SPACE}*
ASSIGN 		 = {VAR}\={SPACE}*([\-]?[\-]({VAR}|{FNAME})+([\=]({VAR}|{FNAME})+)?{SPACE}*)*{SPACE}*
FILE_COMMAND = {GREP_COMMAND} | {BASE_COMMAND} | {ASSIGN}

OPERATOR_RIGHT  = [\>]|[\>][\&]|[\&][\>]|[\>][\>]|[\>][\>][\>]
OPERATOR_LEFT	= [\<]|[\<][\&]|[\&][\<]|[\<][\<]|[\<][\<][\<]
OPERATOR_RL		= [\<][\>]
RIGHT_FILE_REDIRECT = ({OPERATOR_RIGHT}|{OPERATOR_RL}){SPACE}*{FILESTRING}
LEFT_FILE_REDIRECT	= {FILESTRING}{SPACE}*({OPERATOR_LEFT}|{OPERATOR_RL})
REDIRECT_IGNORE		= ([0-2]({OPERATOR_LEFT}|{OPERATOR_RL})) | (({OPERATOR_RIGHT}|{OPERATOR_RL})[0-2])

STRING_ESCAPED = [\\]{STRING}
IGNORE		   = {REDIRECT_IGNORE} | {STRING_ESCAPED} | ([\\][\#]) | "ssh"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;

	private String stringBeginner = ""; 
	private boolean	escapeNext = false;


    public COMFLOWFilePath() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
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
				{FNAME}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{if (!escapeNext) {yybegin(COMMENT);}}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();}
				{IGNORE}		{}
			    {FILEEXIST}		{								    							
			    					int index = yytext().indexOf('-');
									String subfile = yytext().substring(index);
									String name = subfile.replaceAll("\"", "").replaceAll("\\{", "").replaceAll("\\}", "").replaceAll("]", "").split(" ")[1];
    								setError(location,"It is forbidden to use a file name such as " + name + " directly.", yyline+1);
								}
			    {LEFT_FILE_REDIRECT}|{RIGHT_FILE_REDIRECT}	
								{								    							
								    String name = yytext().replaceAll("([\\s]|[\\>]|[\\\"]|[\\']|[\\<]|[\\&]|[\\{]|[\\}])+","");
    								setError(location,"It is forbidden to use a file name such as " + name + " directly.", yyline+1);
								}
			 	{FILE_COMMAND}	{yybegin(FILE);}
				{STRING}		{
									stringBeginner=yytext();
									yybegin(STRING);
								}									
				[^]            	{}
		}
		
/************************/
/*  FILE STATE      	*/
/************************/
<FILE>
		{
				{FILESTRING}+	{
							    	String name = yytext().replaceAll("([\\\"]|[\\']|[\\{]|[\\}]|[\\[]|[\\]])+","");
   							 		setError(location,"It is forbidden to use a file name such as " + name + " directly.", yyline+1);
    								escapeNext=false;
    								yybegin(YYINITIAL);
    							}
				{COMMAND_END}	{escapeNext=false; yybegin(YYINITIAL);}
				{LEFT_FILE_REDIRECT}|{RIGHT_FILE_REDIRECT}	
								{	
									String name = yytext().replaceAll("([\\s]|[\\>]|[\\\"]|[\\']|[\\<]|[\\&]|[\\{]|[\\}])+","");
    								setError(location,"It is forbidden to use a file name such as " + name + " directly.", yyline+1);
	    							escapeNext=false;
	    							yybegin(YYINITIAL);
	    						}
				.				{}
		}

/************************/
/*  STRING STATE      	*/
/************************/
<STRING>
		{
				{ESCAPE}	{
								if(!escapeNext)
								{
									escapeNext=true;
								} else
								{
								 	escapeNext=false;
								}
							}
				{STRING}	{
								if(!escapeNext && yytext().equals(stringBeginner))
								{
									yybegin(YYINITIAL);
								}
								escapeNext=false;
							}
				{COMMENT_WORD}	{escapeNext=false;}
				[^]          	{escapeNext=false;}																		
		}		


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}