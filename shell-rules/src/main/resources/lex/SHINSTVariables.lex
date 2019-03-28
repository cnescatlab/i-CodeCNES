/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.Variables rule. 	  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class SHINSTVariables
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, STRING

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
SPACE		 = [\ \r\t\f]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
NAME	     = [a-zA-Z\_][a-zA-Z0-9\_]*
SHELL_VAR	 = ([0-9]+|[\-\@\?\#\!\_\*\$])

ESCAPE		 = [\\]
STRING		 = [\"]

VAR_NO_ERROR = {NAME}|([\$][\{]({NAME}+|{SHELL_VAR}[\}]))
VAR_ERROR	 = ([\$]({NAME}|{SHELL_VAR}))


																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	private boolean goodPractice = false;
	private boolean escapeNext = true;
	private String stringBeginner = "";
	
    public SHINSTVariables() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
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
/* STRING STATE	    */
/************************/
<STRING>
		{
					{ESCAPE}		{	
										if(!escapeNext){
											escapeNext=true;
										}
										goodPractice=false;
										escapeNext=false;										
									}
				{VAR_ERROR}{STRING}	{
									
										if(stringBeginner.equals(yytext().substring(yytext().length()-1))){
											if(!escapeNext && !goodPractice){
					    						String var = yytext().substring(0,yytext().length()-1);
				    					 		setError(location,"The variable " + var + " is not correctly used (must be used with the ${ } or \" \" notation )", yyline+1);
				    					 	}
				    					 	escapeNext=false;
				    					 	yybegin(YYINITIAL);
				    					}else{
				    						if(!escapeNext){
					    						String var = yytext().substring(0,yytext().length()-1);
				    					 		setError(location,"The variable " + var + " is not correctly used (must be used with the ${ } or \" \" notation )", yyline+1);
				    						}
				    					}
				    					goodPractice=false;
			    					 	escapeNext=false;
			    					}
			   	{VAR_ERROR}			{	if(!escapeNext){
			   								setError(location,"The variable " + yytext() + " is not correctly used (must be used with the ${ } or \" \" notation )", yyline+1);
			   							}
			   							goodPractice=false;
			   							escapeNext=false;
			   						}
			   							
				{STRING}			{
										if(!escapeNext && stringBeginner.equals(yytext())){
											yybegin(YYINITIAL);
										}
										goodPractice=false;
										escapeNext=false;
									}
				[^]				{
										escapeNext=false;
										goodPractice = false;
									}
				
								
									
							
		}
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{ESCAPE}			{	
			  							if(!escapeNext){
			  								escapeNext=true;
			  							}
			  							escapeNext=false;
			  						}
			  	{COMMENT_WORD} 		{
			  							if(!escapeNext){
			  								yybegin(COMMENT);
			  							}
			  							escapeNext=false;
			  						}
			  	{STRING}			{
			  							if(!escapeNext){
			  						   		stringBeginner=yytext();
			  						   		goodPractice = true;
			  						  		yybegin(STRING);
			  						  	}
			  						  	escapeNext=false;
			  						}
			  	{FUNCTION}     		{	
										escapeNext=false;
										yybegin(NAMING);
									}
				{VAR_NO_ERROR}		{}
			    {VAR_ERROR}			{
			    						if(!escapeNext){
			    					 		setError(location,"The variable " + yytext() + " is not correctly used (must be used with the ${ } or \" \" notation )", yyline+1);
			    					 	}
			    					 	escapeNext=false;
			    			 	   	}
				{FUNCT}				{	
										location = yytext().replaceAll("[\\s]+", "");
										location = location.substring(0,location.length()-2);
										escapeNext=false;
									}
			    
			    [^]		      	  	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}