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
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F90DESIGNObsoleteDoShared
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A new state named DO_LINE is created in order to verify the DO rule */
%state COMMENT, NAMING, NEW_LINE, LINE, DO_LINE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
	
	List<String> identifiers = new LinkedList<String>();
	
	public F90DESIGNObsoleteDoShared() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void checkIndentifiers(String id) throws JFlexException {
		Boolean found = false;
		int size = identifiers.size();
		for (int i = 0; i < size; i++) {
			if (id.equals(identifiers.get(i)) && !found) {
				this.setError(location,"Each loop shall have its own END DO. Shared END DO is forbidden.", yyline + 1);
				found = true;
			}
		}
		if (!found) {
			identifiers.add(id);
		}
	}
%}

%eofval{
return getCheckResults();
%eofval}

/* Transition word is else (or ELSE). We also look for DO, END, THEN and IF. */
DO		  = do		 | DO
END       = end      | END
INT		  = [0-9]+

%%          

				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 identifiers.clear();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{identifiers.clear(); yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/* If a DO is found, go to state DO_LINE to verify the rule */
<DO_LINE>		{INT}			{checkIndentifiers(yytext());
								 yybegin(COMMENT);}
<DO_LINE>		{VAR}			{yybegin(LINE);}	
<DO_LINE>    	\n             	{yybegin(NEW_LINE);}
<DO_LINE>		.				{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>	  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{DO}			{yybegin(DO_LINE);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>	  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DO}			{yybegin(DO_LINE);}	
<NEW_LINE>		{INT}			{yybegin(LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{DO}			{yybegin(DO_LINE);}	
<LINE>			{END}			{yybegin(LINE);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]           {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}