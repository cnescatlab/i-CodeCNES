/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**
 * This file is used to generate a rule checker for SH.IO.Redirect rule. 
 * For further information on this, we advise you to refer to RNC manuals.
 * Redirection based on GNU redirect manual page.
 *
 * The different redirection are :
 *
 * - Input redirection (e.g : [n]<word )
 * - Output redirection (e.g : [n]>[|]word
 * - Redirecting standard ouput or standard Error (e.g : &>word , >&word,  >word 2>&1
 * - Appending standard output or standard Error (e.g : 
 *
 *
 * Usual operators :
 * - >&-
 * - <&-
 * - >
 * - <
 * - >>
 * - <<
 * - <<<
 * - >>>
 * - <&digit-
 * - >&digit-
 * - <>
 */
package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHIORedirect
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING

COMMENT_WORD	 = \#
FUNCTION    	 = "function"
FUNCT			 = {VAR}{SPACE}*\(\)
SPACE		 	= [\ \r\t\f]
VAR		     	= [a-zA-Z][a-zA-Z0-9\_]*
STRING		 	= \'[^\']*\' | \"[^\"]*\"
OPERATOR_RIGHT  = [\>]|[\>][\&]|[\&][\>]|[\>][\>]|[\>][\>][\>]
OPERATOR_LEFT	= [\<]|[\<][\&]|[\&][\<]|[\<][\<]|[\<][\<][\<]
OPERATOR_RL		= [\<][\>]
REDIRECT_RIGHT	= ([a-zA-Z0-9\_\?\-\/\\\}]|{STRING})+{SPACE}*({OPERATOR_RIGHT}|{OPERATOR_RL}){SPACE}*([a-zA-Z3-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT_LEFT	= ([a-zA-Z3-9\_\?\-\/\\\}]|{STRING})+{SPACE}*({OPERATOR_LEFT}|{OPERATOR_RL}){SPACE}*([a-zA-Z0-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT_RL		= ([a-zA-Z3-9\_\?\-\/\\\}]|{STRING})+{SPACE}*{OPERATOR_RL}{SPACE}*([a-zA-Z3-9\_\?\.\-\$\/\\]|{STRING})+
REDIRECT 		= {REDIRECT_RIGHT}|{REDIRECT_LEFT}|{REDIRECT_RL}

																
%{
	String location = "MAIN PROGRAM";
	List<String> redirections = new ArrayList<String>();
	private boolean isLastLineCommented = false;
	private boolean errorReported=false;
	
    public SHIORedirect() {
    	
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
				\n             	{
									yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{
									location = yytext(); yybegin(YYINITIAL);}
				\n             	{	
									isLastLineCommented=false;
									yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{
			  						isLastLineCommented = true;
			  						yybegin(COMMENT);
		  						}
			    {REDIRECT}		{if (!isLastLineCommented && !errorReported){
			    					errorReported=true;
			    					setError(location,"The non-standard redirection "+yytext()+" must be preceded by a comment.", yyline+1);
			    				}
			    				}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
	      		.         		{}
	     		\n				{isLastLineCommented=false;errorReported=false;}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}