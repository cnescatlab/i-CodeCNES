/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.ARCH.Include rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*      																	 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DESIGNInclude
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* The word that could sent a error is INCLUDE. We also get the include file, to determine later */
/* if it is a fortran 77 file.																	 */
INCLUDE	 	 = [^a-zA-Z0-9\_]("include"){SPACE}+{STRING}

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	/** Used to determine if a comment is before an include declaration. **/
	boolean comment = false;
	/** Used to determine if the file included is a Fortran 77 file. **/
	boolean rightExtension = false;
	
	public F90DESIGNInclude() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
%}

%eofval{ 
	 
	return getCheckResults();
%eofval}

%%          
			{COMMENT_WORD}	{yybegin(COMMENT);}

/************************************************************************/
/* COMMENT STATE        												*/
/* 																		*/
/* At the end of the comment, the boolean 'comment' is set to true. 	*/
/************************************************************************/
<COMMENT>   	
		{
			\n|\r           {comment = true;
							 yybegin(YYINITIAL);}  
			.              	{}
		}

/************************/
/* NAMING STATE        */
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n|\r           {yybegin(YYINITIAL);}
			.              	{}
		}

/****************************************************************************/
/* YYINITIAL STATE      													*/
/*																			*/
/* Whenever a word (not space character) is met, we set comment to false.	*/
/* When 'include' is found, we thrown an error if :							*/
/*    - there is no comment before											*/
/*    - OR, the file included is fortran 77 file							*/
/****************************************************************************/
<YYINITIAL>  	
		{	
			{STRING}		{comment = false;}
			{FALSE}			{comment = false;}
			{TYPE}         	{comment = false;
							 location = yytext(); 
							 yybegin(NAMING);}
			{INCLUDE}		{rightExtension = !yytext().toLowerCase().contains(".f90") && 
										(yytext().toLowerCase().contains(".f77")
										|| yytext().toLowerCase().contains(".f"));
							 if(!comment || !rightExtension) {
								setError(location,"module may use instead of this inclusion", yyline+1);
							 }
							}
			{SPACE}			{}
			.              	{comment = false;}
			\n|\r			{}
		}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }