/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.PRES.LengthLine rule.	  */
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
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESLengthLine
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING

FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*


																
%{
	String location = "MAIN PROGRAM";
	int length = 0;

    public COMPRESLengthLine() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void checkLine() throws JFlexException {
		if(length > 100) 
			setError(location,"There are more than 100 characters in this line.", yyline+1);
		length = 0;
	}
			
%}

%eofval{
	return getCheckResults();
%eofval}


%%          



/************************/

		
		
/************************/
/* NAMING STATE	    	*/
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); length+=yytext().length(); yybegin(YYINITIAL);}
				\n             	{checkLine(); yybegin(YYINITIAL);}  
			   	.              	{length+=yytext().length();}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				{FUNCTION}     	{length+=yytext().length(); yybegin(NAMING);}
				{FUNCT}			{length+=yytext().length(); location = yytext().substring(0,yytext().length()-2).trim(); }
			    \n				{checkLine();}
	      		.              	{length+=yytext().length();}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}