/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                              */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the MAXImbric.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;


import java.util.logging.Logger;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90METNesting
%extends AbstractChecker
%public
%ignorecase
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, IF_STATE

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
END			 = END		  | end
DO		   	 = DO		  | do
IF			 = IF    	  | if		
THEN		 = THEN		  | then
ELSES        = ELSE[\ ]*IF| else[\ ]*if | ELSE | else
END_CONTROL  = END[\ ]*IF |end[\ ]*if | [0-9]+[\ ]+CONTINUE | [0-9]+[\ ]+continue |
			   END[\ ]*DO | end[\ ]*do
CLE			 = {IF}		  | {DO}	    | {ELSES}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"


%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> identifiers = new LinkedList<String>();
	float numImbrics = 0;
	float numMaxImbrics = 0;
	float numImbricsTotal = 0;
	int functionLine = 0;
	boolean end = true;
	
	public F90METNesting(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void addImbrics() {
		numImbrics++;
		if(numMaxImbrics < numImbrics)  {
			numMaxImbrics = numImbrics;
			numImbricsTotal = numImbrics;
		}
	}
	
	private void deleteImbrics() {
		numImbrics--;
	}
	
	private void endLocation() throws JFlexException {
		final List<CheckResult> list = this.getCheckResults();
        if (list.isEmpty()) {
       		this.computeMetric(this.location, numMaxImbrics, functionLine+1);
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numMaxImbrics);
            } else {
            	this.computeMetric(this.location, numMaxImbrics, functionLine+1);
			}
        }
	}
	
%}

%eofval{
	this.computeMetric(null, Float.NaN, 0);
	return getCheckResults();
%eofval}
%%

/* This is the general automaton. Each part will be described later. */

				{COMMENT_WORD}	{yybegin(COMMENT);}
				
				
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


<NAMING>		{VAR}			{numImbrics = 0; numMaxImbrics = 0;  
								 location = location + " " + yytext(); 
								 yybegin(COMMENT);}							 
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/******************************************************************************/
/* This is the first state of the automaton. The automaton will never go back */
/* to this state after.                                                       */
/******************************************************************************/
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext();functionLine = yyline;
								 yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/******************************************************************************/
/* This state is reached whenever a new line starts.                          */
/******************************************************************************/
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();functionLine = yyline;
								 yybegin(NAMING);}
<NEW_LINE>		{ELSES}			{}
<NEW_LINE>		{DO}			{addImbrics();}
<NEW_LINE>		{IF}			{yybegin(IF_STATE); }
<NEW_LINE>		{END_CONTROL}	{deleteImbrics();}
<NEW_LINE>		{END}			{endLocation();}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext();functionLine = yyline;
								 yybegin(NAMING);}
<LINE>			{ELSES}			{}
<LINE>			{DO}			{addImbrics(); }
<LINE>			{IF}			{yybegin(IF_STATE); }
<LINE>			{END_CONTROL}	{deleteImbrics(); }
<LINE>			{END}			{endLocation();}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<IF_STATE>		{THEN} |{CLE}   {addImbrics(); yybegin(LINE);}
<IF_STATE>		{VAR}			{end=true;}
<IF_STATE>		\&				{end=false;} 
<IF_STATE>		\n 				{if(end) { addImbrics(); deleteImbrics(); yybegin(NEW_LINE);}}
<IF_STATE>		.				{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}