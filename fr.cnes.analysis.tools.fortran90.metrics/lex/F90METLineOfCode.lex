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
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

%%

%class F90METLineOfCode
%extends AbstractMetric
%public
%ignorecase

%line

%function run
%yylexthrow JFlexException
%type FileValue

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
	FileValue fileValue;
	float numLines = 1;
	float numTotal = 1;
	int functionLine = 0;
	
	public F90METLineOfCode(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	private void endLocation() {
		final List<FunctionValue> list =
                this.fileValue.getFunctionValues();
        if (list.isEmpty()) {
            list.add(new FunctionValue(this.location, numLines, functionLine+1));
        } else {
            final FunctionValue last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numLines);
            } else {
				list.add(new FunctionValue(this.location, numLines, functionLine+1));
			}
        }
		
	}
	
%}


/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	fileValue.setValue(numTotal);
	return fileValue;
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


				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
