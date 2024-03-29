/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.TYPE.Integer rule.	 */
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
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;

%%

%class F90TYPEInteger
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* We ha a state called DECL. */
%state COMMENT, NAMING, DECL

COMMENT_WORD = "!"
COMMENT_LINE = "!"[^\n\r]*
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

NO_ERROR     = ( "integer" ){SPACE}*("(")
ERROR		 = ( "integer" )
SELECTED 	 = ("selected_")("int"|"real"|"char")("_kind")

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	/** Boolean to determine if it is a real error **/
	boolean error = false;
	/** Boolean to determine if the variable name needs to be saved **/
	boolean saveVar = true;
	/** String to store the name of the variable who throws the error **/
	String variable = "";
	
	public F90TYPEInteger() {
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

			{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n|\r           {yybegin(YYINITIAL);}  
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

/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>  	
		{	
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{NO_ERROR}		{}
			{ERROR}			{yybegin(DECL);}
			.              	{}
			\n|\r			{}
		}
		
/****************/
/*  DECL STATE	*/
/****************/			
<DECL>
		{
			{COMMENT_LINE}	{}
			{STRING}		{}
			"::"			{error = true;}
			{SELECTED}		{error = false;}
			{VAR}			{if(error && saveVar) {
								if(variable == "") variable = yytext();
								else variable = variable + ", " + yytext();
							 }
							}
			\=				{saveVar = false;}
			.				{}
			\n|\r			{if (error) {
								this.setError(location,"It misses the declaration SELECTED_INT_KIND in the initialisation of " + variable, yyline + 1);
							 }
							 error = false;
							 variable = "";
							 saveVar = true;
							 yybegin(YYINITIAL);
							}
		}
	
/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }