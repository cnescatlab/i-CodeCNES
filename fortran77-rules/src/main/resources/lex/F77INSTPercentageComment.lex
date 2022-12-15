/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/****************************************************************************************/
/* This file is used to generate a rule checker for F77.INST.PercentageComment rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	            */
/* As many comments have been done on the ExampleRule.lex file, this file               */
/* will restrain its comments on modifications.								            */
/*																			            */
/****************************************************************************************/

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F77INSTPercentageComment
%extends AbstractChecker
%public
%column
%line

%ignorecase

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
TYPE         = {FUNC}     | {PROC} | {SUB} | {INTER} | {MOD} | {PROG}
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
CLOSING      = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END          = END        | end
STRING       = \'[^\']*\' | \"[^\"]*\"
SPACE        = [\ \r\t\f]
                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    int commentsLines = 0;
    int numTotal = 0;
	double commentsPercent = 0;
    
    public F77INSTPercentageComment(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
    
    private void checkPercentageComment() {
		commentsPercent = (double)commentsLines / numTotal * 100;
        if(commentsPercent < 30.00) {
            setError(location,"There are less than 30% lines of comments in this file: " + String.format("%,.2f", commentsPercent) + "% (" + commentsLines + " / " + numTotal + ")", yyline+1); 
        }
    }
    
%}
%eofval{
    checkPercentageComment();
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
            \n              {numTotal++; commentsLines++; yybegin(NEW_LINE);}
            .               {}
        }
/*************************/
/* INLINE_COMMENT STATE  */
/*************************/
<INLINE_COMMENT>      
        {
            \n              {numTotal++; commentsLines++; yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* AVOID STATE          */
/************************/
<AVOID>           \n          {numTotal++; yybegin(NEW_LINE);}
<AVOID>           .           {}
/************************/
/* NAMING STATE         */
/************************/
<NAMING>
        {
            {VAR}           {yybegin(AVOID);}
            \n              {numTotal++; yybegin(NEW_LINE);}
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
            {TYPE}              {yybegin(NAMING);}
            {CLOSING}           {yybegin(LINE);}
            {END}               {yybegin(AVOID);}
            {SPACE}             {yybegin(LINE);}
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
            {TYPE}          {yybegin(NAMING);}
            {CLOSING}       {}
            {END}           {yybegin(AVOID);}
            {VAR}           {}
            \n              {numTotal++; yybegin(NEW_LINE);}
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
