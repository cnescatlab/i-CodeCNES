/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 
/***********************************************************************************/
/* This file is used to generate a rule checker for F90.DESIGN.ProcedureLines rule.*/
/* For further information on this, we advise you to refer to RNC manuals.         */
/* As many comments have been done on the ExampleRule.lex file, this file          */
/* will restrain its comments on modifications.                                    */
/*                                                                                 */
/***********************************************************************************/

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

%class F90DESIGNProcedureLines
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, INLINE_COMMENT, PROCEDURES_DEF

COMMENT_WORD = \!
PROCEDURES   = PROCEDURE | procedure | SUBROUTINE | subroutine | FUNCTION | function
PROG         = PROGRAM   | program
MOD          = MODULE    | module
INTER        = INTERFACE | interface
TYPE         = {PROG} | {MOD} | {INTER}
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
CLOSING      = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END          = END[\ ]*{PROCEDURES} | end[\ ]*{PROCEDURES}
STRING       = \'[^\']*\' | \"[^\"]*\"
SPACE        = [\ \r\t\f]
                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    int codeLines = 0;
	boolean procStarted = false;
    
    public F90DESIGNProcedureLines(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
	
	private void checkProcedureCodeLines() {
        if(procStarted && codeLines > 150 ) {
            this.setError(location,"This procedure contains more than 150 lines of code: " + codeLines, yyline+1);
        }
    }
    
%}
%eofval{
    return getCheckResults();
%eofval}
%eofclose
%%          
/************************/
                
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>     
        {
            \n              {yybegin(NEW_LINE);}
            .               {}
        }
/*************************/
/* INLINE_COMMENT STATE  */
/*************************/
<INLINE_COMMENT>   
        {
            \n              {codeLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* AVOID STATE          */
/************************/
<AVOID>           \n          {codeLines++; yybegin(NEW_LINE);}
<AVOID>           .           {}
/************************/
/* NAMING STATE         */
/************************/
<NAMING>
        {
            {VAR}           {yybegin(AVOID);}
            \n              {codeLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>   
        {
            {COMMENT_WORD}  {yybegin(COMMENT);}
            {STRING}        {yybegin(LINE);}
            {TYPE}          {yybegin(NAMING);}
			{PROCEDURES}        {codeLines = 0; location = yytext(); procStarted = true;
                                yybegin(PROCEDURES_DEF);}
            {SPACE}         {}
            \n              {yybegin(NEW_LINE);}
            .               {yybegin(LINE);}
        }
/************************/
/* PROCEDURES_DEF STATE     */
/************************/
<PROCEDURES_DEF>
		{
			{VAR}				{location = location + " " + yytext(); codeLines--; yybegin(AVOID);}
			\n             	   {yybegin(NEW_LINE);}
		 	.              	   {}
		 }
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>    
        {
            {COMMENT_WORD}      {yybegin(COMMENT);}
            {STRING}            {yybegin(LINE);}
            {TYPE}              {yybegin(NAMING);}
            {PROCEDURES}        {codeLines = 0; location = yytext(); procStarted = true;
                                yybegin(PROCEDURES_DEF);}
            {CLOSING}           {yybegin(LINE);}
            {END}               {checkProcedureCodeLines(); procStarted = false;}
            {SPACE}             {}
            \n                  {yybegin(NEW_LINE);}
            .                   {yybegin(LINE);}
        }
/************************/
/* LINE STATE           */
/************************/
<LINE>            
        {
            {COMMENT_WORD}  {yybegin(INLINE_COMMENT);}
            {STRING}        {}
            {TYPE}          { yybegin(NAMING);}
            {PROCEDURES}    {codeLines = 0; location = yytext();
                            yybegin(PROCEDURES_DEF);}
            {CLOSING}       {}
            {END}           {checkProcedureCodeLines(); procStarted = false;}
            {VAR}           {}
            \n              {codeLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* ERROR STATE          */
/************************/
                [^]            {
                                    
                                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
                                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                                    errorMessage, yytext(), yyline, yycolumn);
                                }