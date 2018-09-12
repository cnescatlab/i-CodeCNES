/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.FLOW.CheckUser rule.		  */
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

%class SHFLOWCheckUser
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

OPTION		 = \-[unr] | \-\-"user" | \-\-"name" | \-\-"real"
SPACE_OPTION = {SPACE}+ {OPTION}
DIRECT_USER	 = \$\("id" {SPACE_OPTION}+ \) | \"\$\("id" {SPACE_OPTION}+ \)\"
USER		 = {VAR}\={DIRECT_USER}
OP			 = \=\= | \!\= | "-eq" | "-ne" 
ROOT_VAL	 = "root" | "0"
ROOT_VALUE	 = {ROOT_VAL} | \" {ROOT_VAL} \"
VARIABLE	 = \$ {VAR} |  \$ \{ {VAR} \} | \" \$ {VAR} \" | \" \$ \{ {VAR} \} \" 
ROOT		 = {VARIABLE} {SPACE}+ {OP} {SPACE}+ {ROOT_VALUE}
DIRECT_CHECK = {DIRECT_USER} {SPACE}+ {OP} {SPACE}+ {ROOT_VALUE}

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	int errorLine =  0;
	String errorLocation = "";
	String checkUser = "";

    public SHFLOWCheckUser() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void checkRootUser(String rootUser) {
		// rootUser is like "${current_user}" == "root"
		if(rootUser.contains(checkUser) && location.equals(errorLocation)) {
			errorLine = 0;
			errorLocation = "";
		}
	}
	
	private void checkErrors() throws JFlexException {
		if(errorLine != 0) {
			setError(errorLocation,"The user has not been checked.", errorLine);
		}
		if(checkUser=="") {
			errorLine = 0;
			errorLocation = "MAIN PROGRAM";
			setError(errorLocation,"The user has not been checked.", errorLine);
		}
	}
			
%}

%eofval{
	checkErrors();
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
/* AVOID STATE	    */
/************************/
<AVOID>   	
		{
			   	[^]            	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
				{DIRECT_CHECK}	{errorLine = 0; checkUser="novar"; yybegin(AVOID);} /* the root user has been checked: go to the end */
			    {USER}			{errorLine = yyline+1; errorLocation = location; checkUser = yytext().split("=")[0];}
			    {ROOT}			{checkRootUser(yytext());}
	      		[^]         	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}