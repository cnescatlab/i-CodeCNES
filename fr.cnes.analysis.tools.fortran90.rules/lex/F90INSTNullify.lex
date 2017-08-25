/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Nullify rule.	 */
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
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F90INSTNullify
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, DEALLOC, SET_NULL, NULL_VAR

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*(\%[a-zA-Z][a-zA-Z0-9\_]*)*
STRING		 = \'[^\']*\' | \"[^\"]*\"

DEALLOCATE	 = [^a-zA-Z0-9\_]("deallocate"){SPACE}*("(")
NULLIFY		 = [^a-zA-Z0-9\_]("nullify"){SPACE}*("(")

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	List<String> pointers = new LinkedList<String>(); 
	List<Integer> lines = new LinkedList<Integer>();
	int errorLine = 0;
	boolean isPointer = false;
	
	public F90INSTNullify() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void raiseRemainingErrors() throws JFlexException{
		for (int i = 0; i < pointers.size(); i++){
			setError(location,"It misses the instruction NULLIFY after the DEALLOCATION of" + pointers.get(i), lines.get(i));
		}
		pointers.clear();
		lines.clear();
	}
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
	raiseRemainingErrors();
	 
	return getCheckResults();
%eofval}


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
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}        	{raiseRemainingErrors();
							 location = yytext(); 
							 yybegin(NAMING);}
			{DEALLOCATE}	{errorLine = yyline + 1; yybegin(DEALLOC);}
			{NULLIFY}		{yybegin(NULL_VAR);}
			{VAR}			{if(pointers.contains(yytext())) {
								setError(location,"It misses the instruction NULLIFY after the DEALLOCATION of " + yytext(), lines.get(pointers.indexOf(yytext())));
								lines.remove(pointers.indexOf(yytext()));
								pointers.remove(yytext());
							 }
							}
			\n             	{}
			.              	{}
		}

/************************/
/* DEALLOC STATE        */
/************************/
<DEALLOC>		
		{
			{VAR}			{if(!isPointer) { 
								pointers.add(yytext()); 
								lines.add(errorLine);
								isPointer = true; 
							 } 
							}
			\n				{isPointer = false; yybegin(YYINITIAL);}
			.				{}
		}

/************************/
/* NULL_VAR STATE       */
/************************/
<NULL_VAR>
		{
			{VAR}			{if(pointers.contains(yytext())) {
								lines.remove(pointers.indexOf(yytext()));
								pointers.remove(yytext());
							 }
							}
			\n				{yybegin(YYINITIAL);}
			.				{}
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