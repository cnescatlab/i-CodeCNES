/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the MAXImbric.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.shell.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHMETLineOfCode
%extends AbstractChecker
%public
%ignorecase
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state AVOID, NAMING, FUNCTION, USEFUL

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

%{
	String location = "MAIN PROGRAM";
	List<String> identifiers = new LinkedList<String>();
	float lines=0;
	float linesMain=0;
	float linesTotal=0;
	int brackets = 0;
	int functionLine = 0;

	
	public SHMETLineOfCode(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void endLocation() throws JFlexException {
       	this.computeMetric(this.location, lines+1, functionLine + 1);
	}
	
%}

%eofval{
    final List<FunctionValue> list = this.fileValue.getFunctionValues();
    list.add(new FunctionValue("MAIN PROGRAM", linesMain, functionLine+1));
    fileValue.setValue(linesTotal);
	return fileValue;
%eofval}
%eofval{
	this.computeMetric("MAIN PROGRAM", linesMain, functionLine+1);
	this.computeMetric(null, linesTotal, 0);
	return getCheckResults();
%eofval}
%%

/************************/
	
/************************/
/* AVOID STATE	    */
/************************/
<AVOID>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}	
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{functionLine=yyline;location = yytext(); lines=0; yybegin(USEFUL);}
				\n             	{lines=1; linesTotal++; yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				{COMMENT_WORD}		{yybegin(AVOID);}
				{FUNCTION}     		{yybegin(NAMING);}
				{FUNCT}				{functionLine=yyline;location = yytext().substring(0,yytext().length()-2).trim(); lines=0; yybegin(USEFUL); }
			    \{					{brackets++; yybegin(USEFUL);}
			    \}					{brackets--;
			    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
			    				 	 	endLocation();
			    				 	 	functionLine=0;			    				 	 	
			    				 	 	location = "MAIN PROGRAM";
			    				 	 	lines=0;
			    				 	 	linesTotal++;
			    				 	 	yybegin(AVOID);
			    				 	 } else {
			    				 		yybegin(USEFUL); 
			    				 	}}
	      		\n	| {SPACE}		{}
	      		.					{yybegin(USEFUL);}
		}
		
/************************/
/* USEFUL STATE		    */
/************************/
<USEFUL>
		{
				{FUNCTION}     		{yybegin(NAMING);}
				{FUNCT}				{functionLine=yyline;location = yytext().substring(0,yytext().length()-2).trim(); }
			    \{ 					{brackets++;}
			    \}					{brackets--;
			    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
			    				 	 	endLocation();
                                        functionLine=0;
			    				 	 	location = "MAIN PROGRAM";
			    				 	 	lines=0;
			    				 	 	linesTotal++;
			    				 	 	yybegin(AVOID);
			    				 	}}
			    \\{SPACE}*\n		{}
	      		\n	         		{if(location.equals("MAIN PROGRAM")) linesMain++;
	      							 else lines++;
	      							 linesTotal++; yybegin(YYINITIAL);}
	      		.					{}
		}
		



/************************/
/* ERROR STATE	        */
/************************/
				[^]             {}
				
