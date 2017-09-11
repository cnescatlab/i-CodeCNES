/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.Interpreter rule.	  */
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

%class SHINSTInterpreter
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state AVOID

SPACE		 = [\ \r\t\f]

CORRECT		 = [\#][\!][\ ]*[\/]


																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;

    public SHINSTInterpreter() {
    	
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
/* AVOID STATE	    */
/************************/
<AVOID>   	
		{
				[^]         	{}  
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			    {CORRECT}		{yybegin(AVOID);}
			    {SPACE}	| \n	{}	
	      		.	         	{setError(location,"The first line must declare the interpreter.", yyline+1); yybegin(AVOID);}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}