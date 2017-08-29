/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                              */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the NBLine.lex file, this file 			*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90METLineOfCode
%extends AbstractChecker
%public
%ignorecase
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, AVOID, NAMING, NEW_LINE, LINE

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \! 
FUNC         = "FUNCTION "
PROC         = "PROCEDURE "
SUB          = "SUBROUTINE "
PROG         = "PROGRAM "
MOD          = "MODULE "
INTER		 = "INTERFACE "
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTER}
CLOSING		 = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END			 = END	 	  | end
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* We need two variable to determine overall rate and 2 others to determine 	*/
/* comment's rate on each section (function, procedure, etc.).			*/
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	float numLines = 1;
	float numTotal = 1;
	int functionLine = 0;
	
	public F90METLineOfCode(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	private void endLocation() throws JFlexException {
		final List<CheckResult> list = this.getCheckResults();
        if (list.isEmpty()) {
        	this.computeMetric(this.location, numLines, functionLine + 1);
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numLines);
            } else {
				this.computeMetric(this.location, numLines, functionLine + 1);
			}
        }
		
	}
	
%}


/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	this.computeMetric(null, numTotal, 0);
	return getCheckResults();
%eofval}
%%

/* This is the general automaton. Each part will be described later. */

				{COMMENT_WORD}	{yybegin(COMMENT);}
				
				
<COMMENT>   	\n             	{numLines++; numTotal++; yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<AVOID>   		\n             	{yybegin(NEW_LINE);}  
<AVOID>  	 	.              	{}


<NAMING>		{VAR}			{numLines = 1; numTotal++; location = location + " " + yytext(); 
								 yybegin(AVOID);}							 
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext();functionLine=yyline;
								 yybegin(NAMING);
								 }
<YYINITIAL> 	\n             	{numLines++; numTotal++;
								 yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();functionLine=yyline; yybegin(NAMING);}
<NEW_LINE>		{CLOSING}		{}
<NEW_LINE>		{END}			{numLines++; numTotal++;
							     endLocation();
								 yybegin(AVOID);}
<NEW_LINE>  	\n             	{numLines++; numTotal++; 
                                 yybegin(NEW_LINE);}
<NEW_LINE>  	.              	{yybegin(LINE);}


<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext();functionLine=yyline;
								 yybegin(NAMING);}
<LINE>			{CLOSING}		{}
<LINE>			{END}			{numLines++; numTotal++;
                                 endLocation();
								 yybegin(AVOID);}
<LINE>      	\n             	{numLines++; numTotal++; 
								 yybegin(NEW_LINE);}
<LINE>      	.              	{}


				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}