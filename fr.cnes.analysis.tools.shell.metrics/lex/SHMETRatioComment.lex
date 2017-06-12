/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the RATEComment.lex file, this file 		*/
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

%class SHMETRatioComment
%extends AbstractChecker
%public
%ignorecase
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, AVOID, NAMING, FUNCTION, USEFUL

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*



%{
	String location = "MAIN PROGRAM";
	List<String> linesType = new LinkedList<String>();
	List<Integer> locationsLines = new LinkedList<>();
	List<String> locations = new LinkedList<String>();
	int brackets = 0;

	
	public SHMETRatioComment(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private float getComments(int indexOriginal) {
		float comments = 0;
		// comments before the function -> header
		int index = indexOriginal - 1;
		while (index >= 0 && linesType.get(index).equals("comment")) {
			comments++;
			index--;
		}
		//comments inside the function
		index = indexOriginal + 1;
		while ( index<linesType.size() && !linesType.get(index).equals("finFunction")) {
			if(linesType.get(index).equals("comment")) comments++;
			index++;
		}	
		return comments;
	}
	
	private float getLines(int indexOriginal) {
		float lines = 0;
		// comments before the function -> header
		int index = indexOriginal - 1;
		while (index >= 0 && linesType.get(index).equals("comment")) {
			lines++;
			index--;
		}
		//lines inside the function
		index = indexOriginal + 1;
		while (index<linesType.size() && !linesType.get(index).equals("finFunction")) {
			if(linesType.get(index).equals("line") ||
			   linesType.get(index).equals("comment")) lines++;
			index++;
		}	
		return lines+2;
	}
	
	private void getRateCommentsFunctions() throws JFlexException  {
		// get the rate of comments for the functions
		int functNumber = 0;
		for (int i=0; i<linesType.size(); i++) {
			if (linesType.get(i).equals("function")) {
				// count the number of comments and lines
				float comments = getComments(i);
				float lines    = getLines(i);
				// insert the rate into list
			 	this.computeMetric(locations.get(functNumber), comments/lines, locationsLines.get(functNumber));
       			functNumber++;
			}
		}
		// get the number of comments for the main program
		boolean found = false;
		for (int i=linesType.size()-1; i>0 && !found; i--) {
			if (linesType.get(i).equals("finFunction")) {
				// count the number of comments and lines
				float comments = getComments(i+1);
				float lines    = getLines(i+1);
				// insert the rate into list
				this.computeMetric("MAIN PROGRAM", comments/lines, 1);
       			found=true;
			}
		}
	}
	
	private void getRateCommentsFile() throws JFlexException {
		float comments = 0;
		float lines = 0;
		for (int i=0; i<linesType.size(); i++) {
			if (linesType.get(i).equals("comment")) comments++;
			else if (linesType.get(i).equals("line") ||
			         linesType.get(i).equals("function") ||
			         linesType.get(i).equals("finFunction")) lines++;
		}
		this.computeMetric(null, lines/comments, 0);
	}
	
%}

%eofval{
	getRateCommentsFunctions();
	getRateCommentsFile();
	return getCheckResults();
%eofval}

%%

/************************/


/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{linesType.add("comment"); yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
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
				{VAR}			{location = yytext(); locations.add(location);locationsLines.add(yyline+1);}
				\{				{brackets++;}
				\n             	{linesType.add("function");
								 yybegin(YYINITIAL);
								}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location=yytext().substring(0,yytext().length()-2).trim(); locations.add(location);locationsLines.add(yyline+1); yybegin(NAMING);}
			    {VAR}			{yybegin(USEFUL);}
			    \{				{brackets++; yybegin(USEFUL);}
			   	\}				{brackets--;
		    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
		    				 	 	linesType.add("finFunction");
		    				 	 	location = "MAIN PROGRAM";
		    				 	 	yybegin(AVOID);
		    				 	 } else {
		    				 		yybegin(USEFUL); 
		    				 	}}
				{SPACE}			{}
				\n				{linesType.add("empty");}
	      		.              	{yybegin(USEFUL);}
		}
		
/************************/
/* USEFUL STATE	    	*/
/************************/
<USEFUL>
		{
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location=yytext().substring(0,yytext().length()-2).trim(); locations.add(location);locationsLines.add(yyline+1); yybegin(NAMING);}
			    {VAR}			{}
			    \{				{brackets++;}
		    	\}				{brackets--;
		    				 	 if(brackets==0 && !location.equals("MAIN PROGRAM")) {
		    				 	 	linesType.add("finFunction");
		    				 	 	location = "MAIN PROGRAM";
		    				 	 	yybegin(AVOID);
		    				 	}}
				\n				{linesType.add("line"); yybegin(YYINITIAL);}
	      		.              	{}
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]             {}
