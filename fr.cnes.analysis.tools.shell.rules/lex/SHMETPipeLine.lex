/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.MET.PipeLine rule. 		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHMETPipeLine
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
PIPELINE	 = \|{SPACE}	| \|\n		| \|\&

																
%{
	String location = "MAIN PROGRAM";
	int bracket = 0;
	String type = "";
	/** For each line a string that represent the line type: comment, empty, line **/
	List<String> linesType = new ArrayList<String>();

    public SHMETPipeLine() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/**
	  * Check if the last not empty line is a comment or not
	  * @throws JFlexException 
	  */
	private void checkPipelinesComments(int max) throws JFlexException {
		int index = 1;
		while(index <= max) {
			String current = linesType.get(index);
			if(current.equals("pipeline")){
			 checkPrecedent(index, index+1);
			}
			index++;
		}
	}
	
	/**
	 * Check the line before to the current line
	 * @param index
	 * @param lineError
	 * @throws JFlexException
	 */
	private void checkPrecedent(int index, int lineError) throws JFlexException {
		if(index>0) {
			if (linesType.get(index-1).equals("empty")) checkPrecedent(index-1,lineError);
			else if (linesType.get(index-1).equals("line")) setError(location,"Every pipeline must be preceded by a comment.", lineError);
			else if (linesType.get(index-1).equals("pipeline")) setError(location,"Every pipeline must be preceded by a comment.", lineError);
		}
	}

			
%}

%eofval{
	int index = linesType.size()-1; 
	checkPipelinesComments(index);
	return getViolations();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\!\/"bin"		{type="line";}
				\n             	{linesType.add(type); type="empty"; yybegin(YYINITIAL);}  
			   	.              	{if(type.equals("empty")) type="comment";}
		}
		
		
/************************/
/* NAMING STATE	   		*/
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{linesType.add("line"); type="empty"; yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); if(type.equals("empty")) type="line";}
			    \|\|			{}	/** OR logique **/
			    {PIPELINE}		{type="pipeline";
			    				 if(yytext().contains("\n")) {linesType.add(type); type="empty";} }
				{SPACE}			{}
	      		\n             	{linesType.add(type); type="empty";}
	      		\\{SPACE}*\n	{}
	      		.				{if(type.equals("empty")) type="line";}
		}
		

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}