/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.DATA.CommentVar rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

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

%class F77DATACommentVar
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, YYINITIAL, VARCOMMENT_DEF

COMMENT_WORD = \!         | c          | C     | \*
PROG         = PROGRAM   | program
MOD          = MODULE    | module
INTER        = INTERFACE | interface
TYPE         = {PROG} | {MOD} | {INTER}
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
STRING       = \'[^\']*\' | \"[^\"]*\"
SPACE        = [\ \r\t\f]
VAR_T     = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
VARIABLE	= {VAR_T}({SPACE}*"\*"{SPACE}*([:digit:]+ | \(\*\)))?
WORD 		 = ([:letter:] | [:digit:])+
IMPL 		= implicit | IMPLICIT

                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    int lineComment = 0;
	int lineVar = 0;
    
    public F77DATACommentVar(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
	
	private void checkCommentVar() {
        if(lineComment!=lineVar-1){
            this.setError(location, "This variable is not commented", yyline+1);
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
			{WORD}			{lineComment=yyline;}
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
            {VAR}           {yybegin(AVOID);}
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
            {TYPE}          {yybegin(NAMING);}
            {SPACE}         {}
            \n              {yybegin(NEW_LINE);}
            .               {yybegin(LINE);}
        }
/************************/
/* VARCOMMENT_DEF STATE  */
/************************/
<VARCOMMENT_DEF>
		{
			\({VAR}\)		{}
            {VAR}			{location=yytext(); checkCommentVar();}
			\R{SPACE}*(\* | \&)	{}
			\n             	{yybegin(NEW_LINE);}
		 	.              	{}
		}
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>    
        {
            {COMMENT_WORD}      {yybegin(COMMENT);}
            {VARIABLE}          {lineVar=yyline; yybegin(VARCOMMENT_DEF);}
            {STRING}            {yybegin(LINE);}
			{IMPL}				{yybegin(AVOID);}
            {TYPE}              {yybegin(NAMING);}
            {SPACE}             {}
            \n                  {yybegin(NEW_LINE);}
            .                   {yybegin(LINE);}
        }
/************************/
/* LINE STATE           */
/************************/
<LINE>            
        {
            {VARIABLE}      {lineVar=yyline; yybegin(VARCOMMENT_DEF);}
            {STRING}        {}
            {TYPE}          {yybegin(NAMING);}
            {VAR}           {}
            \n              {yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* ERROR STATE          */
/************************/
                [^]            {
                                    
                                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
                                    throw new JFlexException(this.getClass().getName(), parsedFileName, errorMessage, yytext(), yyline, yycolumn);
                                }