/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DESIGN.Interface rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;
import java.util.LinkedList;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F90DESIGNLogicUnit
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, MODULE_DEF, PROGRAM_DEF


COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC} | {PROC} | {SUB}
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
END   		 = end | END



%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> errors = new LinkedList<String>();
	int numUnits = 0;

	
	public F90DESIGNLogicUnit(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
		
%}

%eofval{ 
	return getCheckResults();
%eofval}

%eofclose
%%          



/************************/

				{COMMENT_WORD}	{yybegin(COMMENT);}


/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID>			\n|\r			{yybegin(NEW_LINE);}
<AVOID>			.				{}


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
			{MOD}			{location = yytext(); yybegin(MODULE_DEF);}
			{PROG}			{location = yytext(); yybegin(PROGRAM_DEF);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* MODULE_DEF STATE     */
/************************/
<MODULE_DEF>
		{
			{PROC}			{yybegin(AVOID);}
			{VAR}			{location = location + " " + yytext();
							numUnits++;
							if(numUnits>1) {
									this.setError(location,"This file contains more than one logical unit (program/module).", yyline+1);
								}
							}
			\n             	{yybegin(NEW_LINE);}
		 	.              	{}
		 }


/************************/
/* PROGRAM_DEF STATE     */
/************************/
<PROGRAM_DEF>
		{
			{VAR}			{location = location + " " + yytext();
							numUnits++;
							if(numUnits > 1) {
									this.setError(location,"This file contains more than one logical unit (program/module).", yyline+1);
								}
							}
			\n             	{yybegin(NEW_LINE);}
		 	.              	{}
		 }


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{STRING}			{yybegin(LINE);}
			{MOD}				{location = yytext(); yybegin(MODULE_DEF);}
			{PROG}				{location = yytext(); yybegin(PROGRAM_DEF);}
			{TYPE}         		{yybegin(LINE);}
			{END}				{yybegin(AVOID);}
			{VAR}				{yybegin(LINE);}
			\n             		{}
			.              		{yybegin(LINE);}
		}


/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{STRING}		{}
			{MOD}			{location = yytext(); yybegin(MODULE_DEF);}
			{PROG}			{location = yytext(); yybegin(PROGRAM_DEF);}
			{TYPE}         	{yybegin(AVOID);}
			{END}			{yybegin(AVOID);}
			{VAR}			{}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}


/************************/
/* ERROR STATE          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }
