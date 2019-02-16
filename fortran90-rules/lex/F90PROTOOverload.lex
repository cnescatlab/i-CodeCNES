/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.PROTO.Overload rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90PROTOOverload
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* States added : 	*/
/*   - OPER_IMPL	*/
/*   - FUNC_NAME	*/
/*   - OP_OVER		*/
%state COMMENT, NAMING, NEW_LINE, LINE, OPER_IMPL, FUNC_NAME, OP_OVER


COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

INT_OP		 = ("interface"){SPACE}+("operator")
END			 = "end"
OPERATEUR	 = "+" | "-" | "*" | "/" | "**"

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	Map<String, String> operateurs = new HashMap<String, String>();
	String op;
	boolean equal = false;
	
	public F90PROTOOverload() {
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
							 op = operateurs.get(yytext());
							 if (op != null) {
								yybegin(OPER_IMPL);
							 } else { 
								yybegin(LINE);
							 }
							}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>  	
		{
			{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{TYPE}         	{location = yytext(); yybegin(NAMING);}
			{INT_OP}		{yybegin(OP_OVER);}
			\n             	{}
			.              	{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{TYPE}         	{location = yytext(); yybegin(NAMING);}
			{INT_OP}		{yybegin(OP_OVER);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* OP_OVER STATE        */
/************************/
<OP_OVER>	
		{
			{OPERATEUR}					{op = yytext();}
			{END}{SPACE}+("function")	{}
			{END}{SPACE}+("interface")	{yybegin(LINE);}
			("function")				{yybegin(FUNC_NAME);}
			\n							{}
			.							{}
		}

/************************/
/* FUNC_NAME STATE        */
/************************/
<FUNC_NAME>
		{
			{VAR}			{operateurs.put(yytext(), op); yybegin(OP_OVER);}
			\n				{yybegin(OP_OVER);}
			.				{}
		}

/************************/
/* OPER_IMPL STATE      */
/************************/
<OPER_IMPL>
		{
			{END}			{yybegin(LINE);}
			\=				{equal = true;}
			{OPERATEUR}		{if(equal) {
								if(!yytext().equals(op)) setError(location,"Overloading operator is not allowed. Overload of " + yytext(), yyline+1);
							 }
							}	
			\n				{equal = false;}
			.				{}
		}
		
/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }