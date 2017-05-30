/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DESIGN.ActiveWait rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;
import java.util.LinkedList;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDESIGNActiveWait
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE

FREE_COMMENT = \!
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

END			 = "end"{SPACE}*"do"	| "continue"
WAIT		 = "sleep" 	| "wait"	| "pause"

%{
	String location = "MAIN PROGRAM";
	List<String> errors = new LinkedList<String>();
	int loop = 0;
	
	public COMDESIGNActiveWait(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
		
%}

%eofval{
	return getCheckResults();
%eofval}


%%          

/************************/

			{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE	        */
/************************/
<NAMING>
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	
		{
			{TYPE}        	{location = yytext(); 
							 yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{STRING}			{yybegin(LINE);}
			{FALSE}				{}
			{TYPE}         		{location = yytext(); 
							 	 yybegin(NAMING);}
			"do"				{loop++;}
			{END}				{loop--;}
			{WAIT}				{if(loop>0) this.setError(location,"This process contains an active wait.", yyline+1);}
			{VAR}				{}
			\n             		{}
			.              		{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{STRING}		{yybegin(LINE);}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			"do"				{loop++;}
			{END}			{loop--;}
			{WAIT}			{if(loop>0) this.setError(location,"This process contains an active wait.", yyline+1);}
			{VAR}			{}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}
			
/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}