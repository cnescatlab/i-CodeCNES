/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.GetOpts rule. 		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.util.logging.Logger;

%%

%class SHINSTGetOpts
%extends AbstractRule
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, STRING

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
PARAMETER   =  [\$](([1-9][0-9]*)|[\*])
VAR			 = ([a-zA-Z][a-zA-Z0-9\_\-]*)|([\$]([\-\@\?\#\!\_\0]|([a-zA-Z]*)|[\{][a-zA-Z]*[\}]))
GETOPT		 = "getopt"
GETOPTS		 = "getopts"
ESCAPE_STRING = [\\]([\']|[\"])

																
%{
    private static final Logger LOGGER = Logger.getLogger(SHINSTGetOpts.class.getName());
	String location = "MAIN PROGRAM";
    String parsedFileName;
    
    
	boolean foundGetOpts = false;
	boolean foundGetOpt = false;
	/** The parameter $1 must be checked in the function. It's true if the call is made inside the function containing a getop or not.*/
	private boolean functionContainGetOpt = false;
	private boolean escapeNext = false;
	private String stringBeginner = "";

	
    public SHINSTGetOpts() {
    	
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
	if (!foundGetOpts && !foundGetOpt){
		setError("MAIN PROGRAM","It is mandatory to use getopts & getopt in the script.", 0);
	}else if(!foundGetOpts){
		setError("MAIN PROGRAM","It is mandatory to use getopts & getopt in the script. (getopts is missing)", 0);
	}else if(!foundGetOpt){
		setError("MAIN PROGRAM","It is mandatory to use getopts & getopt in the script. (getopt is missing)", 0);
	}
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
				{VAR}			{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : VAR \""+yytext()+"\" )");
									location = yytext();
									functionContainGetOpt=false;
									yybegin(YYINITIAL);}
				\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
									functionContainGetOpt=false;
									yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* STRING STATE	    */
/************************/
<STRING>
	{
				{PARAMETER} {
								if(!(functionContainGetOpt && yytext().matches("[\\$][\\1]"))){
										String result = functionContainGetOpt ? "true" : "false";
										LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError (Detection : PARAMETER \""+yytext()+"\" && functionContainGetOpt="+result+" )");
										setError(location,"Parameter "+yytext()+" must be treated with getopt or getopts command.", yyline+1);
								}
							}
			    {GETOPT}	{
			    				foundGetOpt=true;
			    				functionContainGetOpt = true;
			    			}
				[\\]		{
								escapeNext=true;
							}
				[\"]|[\']	{
									if(escapeNext){
										escapeNext=false;
									 }else{
									 
								 		if(stringBeginner.equals(yytext())){
								 			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - STRING -> YYINITIAL (Transition : stringBeginner \""+stringBeginner+"\" )");
								 			yybegin(YYINITIAL);
								 		}
								 	}	
							}
							
				.			{
								escapeNext=false;
							}
	
	}
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{
			  						LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			  						yybegin(COMMENT);}
				{FUNCTION}     	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
									yybegin(NAMING);}
				{PARAMETER}		{
									
									/*
									 * We do not raise error if it's $1  parameter call when a getopts was found because the command require this use.
									 */
									if(!(functionContainGetOpt && yytext().matches("[\\$][1]"))){
										String result = functionContainGetOpt ? "true" : "false";
										LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError (Detection : PARAMETER \""+yytext()+"\" && functionContainGetOpt="+result+" )");
										setError(location,"Parameters must be treated with getopt or getopts command.", yyline+1);}
									}
				{ESCAPE_STRING}	{}
				[\"]|[\']		{
									stringBeginner = yytext();
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> YYINITIAL (Transition : VAR \""+yytext()+"\" )");
									yybegin(STRING);
								}
				{FUNCT}			{}
			    {GETOPT}		{
			    					foundGetOpt=true;
			    					functionContainGetOpt=true;
			    				}
			    {GETOPTS}		{
			    					foundGetOpts = true;
			    				}
			    {VAR}			{}
	      		[^]         	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
			[^]            {}