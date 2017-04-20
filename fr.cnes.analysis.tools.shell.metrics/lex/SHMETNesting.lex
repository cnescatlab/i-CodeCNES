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
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

%%

%class SHMETNesting
%extends AbstractMetric
%public
%line

%function run
%yylexthrow JFlexException
%type FileValue

%state COMMENT, NAMING, FUNCTION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z0-9\_\-]+
STRING		 = \'[^\']*\' | \"[^\"]*\"
OPTCHAR		 = \# | \! | % | \* | @ | \^ | \' | , | \/ | : | = | \+ | \? | \[ | \]
VARCOMP		 = {OPTCHAR} | (\$)?{VAR} | \$\{ {VAR} \}
EXTENDEDVAR	 = \$\{ {VARCOMP}+ \}

IMBRIC		 = "while"		| "for"		| "until"	|
			   "if"			| "case" 
END_IMBRIC	 = "done"		| "fi"		| "esac"
ELSE		 = "else"		| "elif"

IGNORE		 = "EOF" [^]* "EOF"


%{
	String location = "MAIN PROGRAM";
	FileValue fileValue;
	List<String> identifiers = new LinkedList<String>();
	float numImbrics = 0;
	float numMaxImbrics = 0;
	float numImbricsTotal = 0;
	int brackets = 0;
	int functionLine = 0;

	
	public SHMETNesting(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	private void addImbrics() {
		numImbrics++;
		if(numMaxImbrics < numImbrics)  {
			numMaxImbrics = numImbrics;
			if (numImbricsTotal < numMaxImbrics) {
				numImbricsTotal = numMaxImbrics;
			}
		}
	}
	
	private void deleteImbrics() {
		numImbrics--;
	}
	
	private void endLocation() {
		final List<FunctionValue> list = this.fileValue.getFunctionValues();
       	list.add(new FunctionValue(this.location, numMaxImbrics, functionLine+1));
	}
	
%}

%eofval{
    final List<FunctionValue> list = this.fileValue.getFunctionValues();
    list.add(new FunctionValue("MAIN PROGRAM", numMaxImbrics, functionLine+1));
    fileValue.setValue(numImbricsTotal);
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
				{VAR}			{functionLine = yyline; location = yytext(); yybegin(YYINITIAL);}
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
				{FUNCT}				{functionLine = yyline; location = yytext().substring(0,yytext().length()-2).trim(); }
				{STRING}			{}
			    {IMBRIC}			{addImbrics();}
			    {ELSE}				{deleteImbrics(); addImbrics();}
			    {END_IMBRIC}		{deleteImbrics();}
			    {VAR}(\=)?			{}
				{EXTENDEDVAR}		{}
			    \{ (\#)?			{brackets++;}
			    \}					{brackets--;
			    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
			    				 	 	endLocation();
			    				 	 	functionLine=0;
			    				 	 	location = "MAIN PROGRAM";
			    				 	 	numMaxImbrics=0;
			    				 	 	numImbrics=0;
			    				 	}}
			    {VAR}				{}
				{IGNORE}			{}
	      		. | \n         		{}
		}
		



/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}
				
