/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77BLOCLoop rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F90DESIGNObsoleteDoEnding
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>

/* A new state named DO_LINE is created in order to store the label if there is one. */
/* A state called ENDING is used to determine on which instruction the label branches. */
%state COMMENT, NAMING, NEW_LINE, LINE, DO_LINE, ENDING

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
	/** List of labels found during analysis, following a DO. **/
	List<String> labels = new LinkedList<String>();
	
	public F90DESIGNObsoleteDoEnding() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void addLabel(String label) throws JFlexException {
		if (!labels.contains(label)) {
			labels.add(label);
		}
	}
%}

%eofval{
return getViolations();
%eofval}

/* The word to catch are DO, END DO and CONTINUE. **/
DO		  = [^a-zA-Z0-9\_]("do")[^a-zA-Z0-9\_]
END       = [^a-zA-Z0-9\_]("end"){SPACE}*("do")?
CONTINUE  = [^a-zA-Z0-9\_]("continue")[^a-zA-Z0-9\_]
INT		  = [0-9]+

%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 labels.clear();
							 yybegin(COMMENT);}
			\n             	{labels.clear();
							 yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* DO_LINE STATE        */
/************************/
<DO_LINE>		
		{
			{INT}			{addLabel(yytext());
							 yybegin(COMMENT);}
			{SPACE}			{}
			{VAR}			{yybegin(COMMENT);}
			\n             	{yybegin(NEW_LINE);}
			.				{yybegin(COMMENT);}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>  	
		{
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{DO}			{yybegin(DO_LINE);}	
			{INT}			{if (labels.contains(yytext())){
								labels.remove(yytext());
								yybegin(ENDING);
							 } else {
								yybegin(LINE);
							 }
							}
			\n             	{}
			.              	{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{DO}			{yybegin(DO_LINE);}	
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* ENDING STATE         */
/************************/
<ENDING>		
		{
			{SPACE}			{}
			{CONTINUE}		{yybegin(COMMENT);}
			{END}			{yybegin(COMMENT);}
			\n|\r			{yybegin(NEW_LINE);}
			.				{setError(location,"A DO loop shall end with END DO. ", yyline + 1); 
							 yybegin(COMMENT);}
		}

/************************/
/* THROW ERROR          */
/************************/
			[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
