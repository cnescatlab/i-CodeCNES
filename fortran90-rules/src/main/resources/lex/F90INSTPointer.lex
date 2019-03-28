/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Pointer rule.	 */
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

import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;

%%

%class F90INSTPointer
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* We had AVOID state, used to avoid part of a line without declaring it has a comment. */
%state COMMENT, NAMING, AVOID, NEW_LINE


COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

POINTER		 = "pointer"
DIMENSION	 = "dimension"	| \( \: (\,\:)* \)	
TIPUS		 = "type"


%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	boolean dimension = false, type = false, pointer = false;

	
	public F90INSTPointer() {
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
/* COMMENT STATE		*/
/************************/
<COMMENT>   	
		{
			\n|\r          	{yybegin(NEW_LINE);}  
			.              	{}
		}
		
/************************/
/* AVOID STATE     	   	*/
/************************/
<AVOID>   	
		{
			\n|\r          	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(AVOID);}
			\n|\r          	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}			{location = yytext(); 
							 yybegin(NAMING);}
			{POINTER}		{pointer=true;}
			{DIMENSION}		{dimension=true;}
			{TIPUS}			{type=true;}
			{VAR}			{}
			\n|\r			{if(pointer && !dimension && !type) setError(location,"This use of POINTER is not allowed.", yyline+1);
							 pointer= false; dimension=false; type=false;}
			. 				{}
		}
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		
		{
			{STRING}		{yybegin(YYINITIAL);}
			{FALSE}			{yybegin(YYINITIAL);}
			{TYPE}			{location = yytext(); 
							 yybegin(NAMING);}
			{SPACE}			{}				 
			{POINTER}		{pointer=true;}
			{DIMENSION}		{dimension=true;}
			{TIPUS}			{type=true;}
			{VAR}			{}
			\n|\r			{if(pointer && !dimension && !type) setError(location,"This use of POINTER is not allowed.", yyline+1);
							 pointer= false; dimension=false; type=false;}
			. 				{yybegin(YYINITIAL);}
		}
		
		
/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }