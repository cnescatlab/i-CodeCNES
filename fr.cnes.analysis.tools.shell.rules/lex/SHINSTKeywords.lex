/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.DATA.Keywords rule. 		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

import java.util.logging.Logger;

%%

%class SHINSTKeywords
%extends AbstractRule
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, STRING, COMMAND


COMMENT_WORD = [\#]
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+\^\*]+
SPACE		 = [\ \r\t\f]
ESCAPE		 	= [\\]
ESCAPE_STRING 	= ([\\][\"]) | ([\\][\'])
STRING		 	= [\'] | [\"]
COMMAND_ENTRY	= ([\$][\(]) | [\`]
COMMAND_EXIT	= ([\)] | [\'])
KEYWORD	 = "case"	| "do"		| "done"	| "elif"	|
			   "else"	| "esac"	| "fi"		| "for"		|
			   "if"		| "in"		| "then"	| "until"	|
			   "while"
KEYWORD_VAR = ([\$][\{]({KEYWORD}[\}])|([\$]{KEYWORD}))
ERROR		 = ({KEYWORD}[\=]) | {KEYWORD_VAR}
KEYWORD_VAR_IGNORE = ([\$][a-zA-Z0-9\_]+{KEYWORD}) | ([\$]{KEYWORD}[a-zA-Z0-9\_]+)
NO_ERROR = (([a-zA-Z0-9\_]+{KEYWORD}|{KEYWORD}[a-zA-Z0-9\_]+)[\=])| {KEYWORD_VAR_IGNORE}

%{
    private static final Logger LOGGER = Logger.getLogger(SHINSTKeywords.class.getName());
	String location = "MAIN PROGRAM";
    String parsedFileName;
    
	private boolean inString = false, escapeNext=false;
	private String stringBeginner="", commandBeginner="";
	
	private ArrayList<String> keywords = new ArrayList<>(Arrays.asList("case"	, "do"		, "done"	, "if"	,
			   "else"	, "esac"	, "fi"		, "for"		,
			   "elif"		, "in"		, "then"	, "until"	,
			   "while"));

    public SHINSTKeywords() {
    	
    }
	
	private boolean isCommandExit(String commandBeginner, String commandExit){
		return (commandBeginner.equals("$(") && commandExit.equals(")")) || (commandBeginner.equals(commandExit));
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.fine("begin method setInputFile");
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(file.toOSString());
        LOGGER.fine("end method setInputFile");
	}
			
%}

%eofval{
	return getViolations();
%eofval}


%%          



/************************/



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
				{FNAME}			{
									
									location = yytext(); 
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : FNAME \""+yytext()+"\" )");
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
			  	{COMMENT_WORD} 	{
			  						LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : VAR \""+yytext()+"\" )");
			  						yybegin(COMMENT);}
				{FUNCTION}     	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : VAR \""+yytext()+"\" )");
									yybegin(NAMING);}
				{FUNCT}			{
									location = yytext().replaceAll("[\\s]+", "");
									location = location.substring(0,location.length()-2);
								}
				{ESCAPE_STRING}	{}
				{STRING}		{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> STRING (Transition : VAR \""+yytext()+"\" )");
									stringBeginner=yytext();
									yybegin(STRING);
								}
				{NO_ERROR}		{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] NO_ERROR \""+yytext()+"\" )");
								}
			    {ERROR}			{
				    				String keyword = "undefined";
				                    if (keywords.contains(yytext().substring(0, yytext().length() - 1))) {
				                        keyword = yytext().substring(0, yytext().length() - 1);
				                    } else {
				                        keyword = yytext().replaceAll(".*[\\$]", "");
				                        for (String listk : keywords) {
				                            if (keyword.contains(listk))
				                                keyword = listk;
				                        }
				                    }
				                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [YYINITIAL] setError() for "+keyword+" in \""+yytext()+"\" )");
				                    setError(location,
				                                "The keywords " + keyword + " cannot be used as a variable.",
				                                yyline + 1);
			    				}
	      		[^]         	{}
		}
/************************/
/* STRING STATE	  	    */
/************************/
<STRING>
		{
				{KEYWORD_VAR_IGNORE}	{	
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [STRING] NO_ERROR \""+yytext()+"\" )");
											escapeNext=false;}
				{KEYWORD_VAR}			{
											escapeNext=false;
											String keyword = "undefined";
						                    if (keywords.contains(yytext().substring(0, yytext().length() - 1))) {
						                        keyword = yytext().substring(0, yytext().length() - 1);
						                    } else {
						                        keyword = yytext().replaceAll(".*[\\$]", "");
						                        for (String listk : keywords) {
						                            if (keyword.contains(listk))
						                                keyword = listk;
						                        }
						                    }
						                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [STRING] setError() for "+keyword+" in \""+yytext()+"\" )");
						                    setError(location,
						                                "The keywords " + keyword + " cannot be used as a variable.",
						                                yyline + 1);
										}	
						
				{ESCAPE}				{
											if(escapeNext){
												LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [STRING] escape next character.)");
												escapeNext=false;
											}else{
												escapeNext=true;
											}
										}
				{STRING}				{
											if(!escapeNext && stringBeginner.equals(yytext())){
												LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : STRING \""+yytext()+"\" )");
												yybegin(YYINITIAL);
											}
											
											escapeNext=false;
										}
				{COMMAND_ENTRY}			{
											inString=true;
											escapeNext=false;
											commandBeginner=yytext();
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - STRING -> COMMAND (Transition : COMMAND_ENTRY \""+yytext()+"\" )");
											yybegin(COMMAND);
										}
										
				[^]					{	escapeNext=false;}
				
		}

/************************/
/* COMMAND STATE	    */
/************************/
<COMMAND>
		{
				{NO_ERROR}		{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] NO_ERROR \""+yytext()+"\" )");	
								}
				{ERROR}			{
									String keyword = "undefined";
				                    if (keywords.contains(yytext().substring(0, yytext().length() - 1))) {
				                        keyword = yytext().substring(0, yytext().length() - 1);
				                    } else {
				                        keyword = yytext().replaceAll(".*[\\$]", "");
				                        for (String listk : keywords) {
				                            if (keyword.contains(listk))
				                                keyword = listk;
				                        }
				                    }
				                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [COMMAND] setError() for "+keyword+" in \""+yytext()+"\" )");
				                    setError(location,
				                                "The keywords " + keyword + " cannot be used as a variable.",
				                                yyline + 1);
								}
				{STRING}		{}
				{COMMAND_EXIT}	{
									if(isCommandExit(commandBeginner, yytext())){
										if(inString){
											inString=false;
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> STRING (Transition : COMMAND_EXIT \""+yytext()+"\" )");
											yybegin(STRING);
										}else{
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : COMMAND_EXIT \""+yytext()+"\" )");
											yybegin(YYINITIAL);
										}
									}
								}

				[^]			{}
		}
/************************/
/* ERROR STATE	        */
/************************/
			[^]            {}