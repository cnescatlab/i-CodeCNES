/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.DESIGN.Bash rule. 		  */
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

%%

%class SHDESIGNBash
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state AVOID

SPACE		 = [\ \r\t\f]

CORRECT		 = \#\!\/"bin"\/"bash"	|
			   \#\!\/"bin"\/"ksh"	|
			   \#\!\/"bin"\/"sh"	|
			   \#\!\/"bin"\/"false"


																
%{
	String location = "MAIN PROGRAM";

    public SHDESIGNBash() {
    	
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
			
%}

%eofval{
	return getViolations();
%eofval}


%%          



/************************/



/************************/
/* AVOID STATE	    */
/************************/
<AVOID>   	
		{
				\n | .         	{}  
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			    {CORRECT}		{yybegin(AVOID);}
			    {SPACE}	| \n	{}	
	      		.	         	{setError(location,"The first line must declare the interpreter (/bin/bash, /bin/ksh or /bin/false)", yyline+1); yybegin(AVOID);}
		}


/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}