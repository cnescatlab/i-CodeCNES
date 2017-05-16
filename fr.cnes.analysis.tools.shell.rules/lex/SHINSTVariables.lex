/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.Variables rule. 	  */
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

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHINSTVariables
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


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
	private boolean goodPractice = false;
	private boolean escapeNext = true;
	private String stringBeginner = "";
	
    public SHINSTVariables() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
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
			  	{VAR_NO_ERROR}		{}
			    {VAR_ERROR}			{
			    						if(!escapeNext){
			    					 		setError(location,"The variable " + yytext() + " is not correctly used (must be used with the ${ } or \" \" notation )", yyline+1);
			    					 	}
			    					 	escapeNext=false;
			    			 	   	}
				{FUNCTION}     		{	
										escapeNext=false;
										yybegin(NAMING);
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
				[^]            {}