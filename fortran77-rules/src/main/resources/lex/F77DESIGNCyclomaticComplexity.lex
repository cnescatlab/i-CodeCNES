/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 
/********************************************************************************************/
/* This file is used to generate a rule checker for F77.DESIGN.CyclomaticComplexity rule.   */
/* For further information on this, we advise you to refer to RNC manuals.                  */
/* As many comments have been done on the ExampleRule.lex file, this file                   */
/* will restrain its comments on modifications.                                             */
/*                                                                                          */
/********************************************************************************************/
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
%class F77DESIGNCyclomaticComplexity
%extends AbstractChecker
%public
%column
%line
%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, NAMING, NEW_LINE, LINE, AVOID
COMMENT_WORD = \!         | c          | C     | \*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTER        = INTERFACE  | interface
TYPE         = {PROG} | {MOD} | {INTER}
PROCEDURES   = {FUNC} | {PROC} | {SUB}
UNION        = \.AND\. | \.and\. | \.OR\. | \.or\.
CICLO        = DO | do | IF | if | ELSE[\ ]*IF | else[\ ]*if
CLOSING         = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
END          = END 		| end
STRING       = \'[^\']*\' | \"[^\"]*\"
                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    int numCyclomatic = 1;
    int procedureLine = 0;
    
    public F77DESIGNCyclomaticComplexity(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
    
    private void checkTotalComplexity() {
        if(numCyclomatic > 15 ) {
            setError(location,"The cyclomatic complexity of this function is more than 15: " +numCyclomatic, procedureLine+1); 
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
/************************/
/* AVOID STATE          */
/************************/
<AVOID>           \n          {yybegin(NEW_LINE);}
<AVOID>           .           {}
/************************/
/* NAMING STATE         */
/************************/
<NAMING>
        {
            {VAR}           {location = location + " " + yytext(); yybegin(AVOID);}
            \n              {yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>   
        {
            {COMMENT_WORD}  {yybegin(COMMENT);}
            {STRING}        {yybegin(LINE);}
            {TYPE}          {yybegin(AVOID);}
            {PROCEDURES}    {numCyclomatic = 1; location = yytext(); procedureLine = yyline; yybegin(NAMING);}
            \n              {yybegin(NEW_LINE);}
            .               {yybegin(LINE);}
        }
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>    
        {
            {COMMENT_WORD}      {yybegin(COMMENT);}
            {STRING}            {yybegin(LINE);}
            {TYPE}              {yybegin(AVOID);}
            {PROCEDURES}        {numCyclomatic = 1; location = yytext(); procedureLine = yyline; yybegin(NAMING);}
            {CICLO}             {numCyclomatic++; yybegin(LINE);}
            {UNION}             {numCyclomatic++; yybegin(LINE);}
            {CLOSING}           {yybegin(LINE);}
            {END}               {checkTotalComplexity();}
            {VAR}               {yybegin(LINE);}
            \n                  {}
            .                   {yybegin(LINE);}
        }
/************************/
/* LINE STATE           */
/************************/
<LINE>            
        {
            {COMMENT_WORD}  {yybegin(COMMENT);}
            {STRING}        {}
            {TYPE}          {yybegin(AVOID);}
            {PROCEDURES}    {numCyclomatic = 1; location = yytext(); procedureLine = yyline; yybegin(NAMING);}
            {CICLO}         {numCyclomatic++;}
            {UNION}         {numCyclomatic++;}
            {CLOSING}       {}
            {END}           {checkTotalComplexity();}
            {VAR}           {}
            \n              {yybegin(NEW_LINE);}
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
