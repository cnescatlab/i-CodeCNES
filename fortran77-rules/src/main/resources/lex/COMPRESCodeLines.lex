/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 
/********************************************************************************/
/* This file is used to generate a rule checker for COM.PRES.CodeLines rule.   */
/* For further information on this, we advise you to refer to RNC manuals.      */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.                                 */
/*                                                                              */
/********************************************************************************/
package fr.cnes.icode.fortran77.rules;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;
import java.util.LinkedList;
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
%%
%class COMPRESCodeLines
%extends AbstractChecker
%public
%column
%line
%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, INLINE_COMMENT
COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTER        = INTERFACE  | interface
TYPE         = {FUNC}     | {PROC}     | {SUB} | {PROG} | {MOD} | {INTER}
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
CLOSING      = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END          = END        | end
STRING       = \'[^\']*\' | \"[^\"]*\"
SPACE        = [\ \r\t\f]
BLANK_LINE 	 = {SPACE}*\R 
                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    int codeLines = 0;
    int numTotal = 1;
    
    public COMPRESCodeLines(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
    
    private void checkTotalCodeLines() {
        if(codeLines > 1000) {
            setError(location,"There are more than 1000 lines of code in this file: " + codeLines, yyline+1); 
        }
    }
    
%}
%eofval{
    checkTotalCodeLines();
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
            \n              {numTotal++; yybegin(NEW_LINE);}
            .               {}
        }
/*************************/
/* INLINE_COMMENT STATE  */
/*************************/
<INLINE_COMMENT>      
        {
            \n              {numTotal++; codeLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* AVOID STATE          */
/************************/
<AVOID>           \n          {numTotal++; codeLines++; yybegin(NEW_LINE);}
<AVOID>           .           {}
/************************/
/* NAMING STATE         */
/************************/
<NAMING>
        {
            {VAR}           {location = location + " " + yytext(); yybegin(AVOID);}
            \n              {numTotal++; codeLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>   
        {
            {COMMENT_WORD}  {yybegin(COMMENT);}
            {STRING}        {yybegin(LINE);}
            {TYPE}          {location = yytext(); yybegin(NAMING);}
            {SPACE}         {}
            \n              {numTotal++; yybegin(NEW_LINE);}
            .               {yybegin(LINE);}
        }
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>    
        {
            {COMMENT_WORD}      {yybegin(COMMENT);}
            {STRING}            {yybegin(LINE);}
            {TYPE}              {location = yytext(); yybegin(NAMING);}
            {CLOSING}           {yybegin(LINE);}
            {END}               {yybegin(AVOID);}
			{BLANK_LINE}        {}
			{SPACE}				{yybegin(LINE);}
            \n                  {numTotal++; yybegin(NEW_LINE);}
            .                   {yybegin(LINE);}
        }
/************************/
/* LINE STATE           */
/************************/
<LINE>            
        {
            {FREE_COMMENT}  {yybegin(INLINE_COMMENT);}
            {STRING}        {}
            {TYPE}          {location = yytext(); yybegin(NAMING);}
            {CLOSING}       {}
            {END}           {yybegin(AVOID);}
            {VAR}           {}
            \n              {numTotal++; codeLines++; yybegin(NEW_LINE);}
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