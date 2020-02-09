/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Only rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F90INSTOnly
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* */
%state COMMENT, NAMING, NEW_LINE, LINE, ONLY_STATE


COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

ONLY		 = "only"

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	List<String> intrinseques =  new LinkedList<String>();
	boolean comment = false;
	
	public F90INSTOnly() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	
	
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
	return getCheckResults();
%eofval}


%%          
		{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n	           {comment=true; yybegin(YYINITIAL);}  
			.              	{}
		}

/************************/
/* NAMING STATE        */
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n	            {comment=false; yybegin(YYINITIAL);}
			.              	{}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>  	
		{	
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); yybegin(NAMING);}
			{ONLY}			{if(!comment) setError(location,"The instruction ONLY must be preceded by a comment", yyline+1);}
			.              	{}
			\n				{comment=false;}
		}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }