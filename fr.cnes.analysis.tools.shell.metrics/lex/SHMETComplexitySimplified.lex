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

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

%%

%class SHMETComplexitySimplified
%extends AbstractMetric
%public
%ignorecase
%line

%function run
%yylexthrow JFlexException
%type FileValue

%state COMMENT, NAMING, FUNCTION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

CYCLO		 = "while"		| "for"		| "until"	|
			   "if"			| "case" 	| "else"	| 
			   "elif"


%{
	String location = "MAIN PROGRAM";
	FileValue fileValue;
	List<String> identifiers = new LinkedList<String>();
	float numCyclo = 0;
	float numMaxCyclo = 0;
	float numCycloTotal = 0;
	int brackets = 0;
	int functionLine = 0;

	
	public SHMETComplexitySimplified(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void addImbrics() {
		numCyclo++;
		if(numMaxCyclo < numCyclo)  {
			numMaxCyclo = numCyclo;
			numCycloTotal = numCyclo;
		}
	}
	
	private void endLocation() {
		final List<FunctionValue> list = this.fileValue.getFunctionValues();
       	list.add(new FunctionValue(this.location, numMaxCyclo+1, functionLine+1));
	}
	
%}

%eofval{
    final List<FunctionValue> list = this.fileValue.getFunctionValues();
    list.add(new FunctionValue("MAIN PROGRAM", numMaxCyclo+1, functionLine+1));
    fileValue.setValue(Float.NaN);
	return fileValue;
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
				{VAR}			{functionLine=yyline;location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 		{yybegin(COMMENT);}
				{FUNCTION}     		{yybegin(NAMING);}
				{FUNCT}				{functionLine=yyline;location = yytext().substring(0,yytext().length()-2).trim(); }
				{STRING}			{}
			    {CYCLO}				{addImbrics();}
			    {VAR}(\=)?			{}
			    \{ (\#)?			{brackets++;}
			    \}					{brackets--;
			    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
			    				 	 	endLocation();
			    				 	 	functionLine=0;			    				 	 	
			    				 	 	location = "MAIN PROGRAM";
			    				 	 	numMaxCyclo=0;
			    				 	 	numCyclo=0;
			    				 	}}
	      		. | \n         		{}
		}
		



/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}
				
