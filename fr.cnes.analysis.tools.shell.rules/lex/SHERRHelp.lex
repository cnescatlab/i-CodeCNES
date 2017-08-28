/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.ERR.Help rule.	 		  */
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

%class SHERRHelp
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CASE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

GETOPTS		 = "getopts" | "getopt"
CASE		 = "case"
ESAC		 = "esac"
HELP_SHORT	 = "h"
HELP	 	 = "help"


																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/* getopts = true if getopts or getopt has been found */
	/* inGetopts = true if we are in the process of examining the different opts */
	/* There will be violations if no getopts has been found at the end (getopts = false) */
	/* and within the case during option examination if h is not treated */
	Boolean getopts = false, inGetopts = false, help = false, help_short = false;
	int lineError = 0;

    public SHERRHelp() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
			
%}

%eofval{
	if (getopts && !help && !help_short) {
		if (lineError == 0) location = "MAIN PROGRAM";
		setError(location,"The help options (-h & --help) must be implemented.", lineError);
	} else if (getopts && !help ) {
		if (lineError == 0) location = "MAIN PROGRAM";
		setError(location,"The help options (-h & --help) must be implemented. Long option --help is missing.", lineError);
	} else if (getopts && !help_short ) {
		if (lineError == 0) location = "MAIN PROGRAM";
		setError(location,"The help options (-h & --help) must be implemented. Short option -h is missing.", lineError);
	} else if(!getopts){
		if (lineError == 0) location = "MAIN PROGRAM";
		setError(location,"The help options (-h & --help) must be implemented in getopt & getopts commands.", lineError);
	}
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
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
			    {GETOPTS}		{getopts=true; inGetopts = true;}
			    {CASE}			{if(inGetopts) {inGetopts = false; lineError=yyline+1; yybegin(CASE);} }
			    {VAR}			{}
	      		[^]        	{}
		}


/************************/
/* CASE STATE	    */
/************************/
<CASE>   	
		{
				.*{HELP}.*[\)] 		{help=true;}
				.*{HELP_SHORT}.*[\)]	{help_short=true;}
				{STRING}			{ }
				{ESAC}         	 {yybegin(YYINITIAL);}
				{VAR}			 {}  
			   	[^]         	 {}
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}