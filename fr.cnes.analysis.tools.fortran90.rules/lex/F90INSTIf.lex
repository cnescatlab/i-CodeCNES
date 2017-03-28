/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.If rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F90INSTIf
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* We had 2 states :	*/
/*		-				*/
/*		-				*/
%state COMMENT, NAMING, NEW_LINE, LINE, IF_EXEC

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

IF			 = [^a-zA-Z0-9\_]("if")[^a-zA-Z0-9\_]
THEN		 = [^a-zA-Z0-9\_]("then")
END_IF		 = [^a-zA-Z0-9\_]("end"){SPACE}*("if")
BRANCH		 = [^a-zA-Z0-9\_]("exit" | "cycle" | "goto" | "return")[^a-zA-Z0-9\_]

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	boolean hasBranch = false;
	boolean hasThen = false;
	boolean endLine = true;
	int errorLine = 0;
	
	public F90INSTIf() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	
%}

%eofval{ 
	 
	return getViolations();
%eofval}


%%          
			{COMMENT_WORD}	{yybegin(COMMENT);}

/********************/
/* COMMENT STATE	*/
/********************/
<COMMENT>   	
		{
			\n|\r           {yybegin(YYINITIAL);}  
			.              	{}
		}


/****************/
/* NAMING STATE	*/
/****************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n|\r			{yybegin(YYINITIAL);}
			.              	{}
		}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{END_IF}		{}
			{IF}			{errorLine = yyline + 1;
							 yybegin(IF_EXEC);}
			\n|\r			{}
			.              	{}
		}
	
/************************/
/* IF_EXEC STATE        */
/************************/
<IF_EXEC>		
		{
			{BRANCH}			{hasBranch = true;}
			{THEN}				{hasThen = true;}
			&{SPACE}*[^\n\r]	{}
			&					{endLine = false;}
			\n|\r				{if (endLine){
									if (!hasThen && !hasBranch) {
										this.setError(location,"Logical IF (without THEN and ENDIF) is only allowed with EXIT, CYCLE, GOTO, RETURN statements.", errorLine);
									}
									hasBranch = false;
									hasThen = false;
									yybegin(YYINITIAL);
								 }
								 endLine = true;
								}
			.					{}
		}
	
/****************/
/* THROW ERROR	*/
/****************/
			[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}