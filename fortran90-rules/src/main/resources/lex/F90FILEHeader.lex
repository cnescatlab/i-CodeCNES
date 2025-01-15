/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.FILE.Header rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F90FILEHeader
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>
%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, YYINITIAL, HEADER

COMMENT_WORD = \!
PROG         = PROGRAM   | program
MOD          = MODULE    | module
MODPROG      = {PROG} | {MOD}
VAR          = [a-zA-Z][a-zA-Z0-9\_]*
STRING       = \'[^\']*\' | \"[^\"]*\"
SPACE        = [\ \r\t\f]
END          = END|  end
COMPONENT    = "COMPONENT NAME" | "Component Name" | "Component name"
FILE         = FILE | File
AUTHOR       = AUTHOR | Author
COPYRIGHT    = COPYRIGHT | Copyright
DESCRIPTION  = DESCRIPTION | Description
ENDHEADER    = use | USE | implicit | IMPLICIT
ELEM_DESC	 = {SPACE}*":"{SPACE}*\R?[\(\)\{\}\[\]\<\>\.\*\+\?\/\"\!,-_:\ \r\t\f]*(\w{SPACE}*)+

                                                                
%{
    String location = "MAIN PROGRAM";
    private String parsedFileName;
    boolean hasComponent = false;
    boolean hasFile = false;
	boolean hasAuthor = false;
    boolean hasCopyright = false;
    boolean hasDescription = false;
    boolean startProgMod = false;
    int line = 0;
	String[] missingData = new String[5];
	int pos = 0;
    
    public F90FILEHeader(){
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
    }
	
	private void checkFileHeader() {
        if(startProgMod == true && hasComponent == false && hasFile == false && hasAuthor == false && hasCopyright == false && hasDescription == false){
            this.setError(location, "This PROGRAM or MODULE must have a header with: component name, file, author, copyright and description.", line);
        }
        else if(startProgMod == true && (hasComponent == false || hasFile == false || hasAuthor == false || hasCopyright == false || hasDescription == false)){
			String message = "Missing data in the header of this PROGRAM or MODULE: ";
			if(!hasComponent) {missingData[pos] = "component name"; pos++;}
			if(!hasFile) {missingData[pos] = "file name"; pos++;}
			if(!hasAuthor) {missingData[pos] = "author"; pos++;}
			if(!hasCopyright) {missingData[pos] = "copyright information"; pos++;}
			if(!hasDescription) {missingData[pos] = "description"; pos++;}
			for(int i = 0; i < pos-1; i++) message += missingData[i] + ", ";
			message += missingData[pos-1] + ".";
            this.setError(location, message, line); 
        }
		pos = 0;
        hasComponent = false;
        hasFile = false;
	    hasAuthor = false;
        hasCopyright = false;
        hasDescription = false;
        startProgMod = false;
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
<AVOID>           \n          {if(startProgMod == true) yybegin(HEADER); if(startProgMod == false) yybegin(NEW_LINE);}
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
            {MODPROG}       {location=yytext(); line=yyline+1; startProgMod = true; yybegin(NEW_LINE);}
            \n              {yybegin(NEW_LINE);}
            .               {yybegin(LINE);}
        }
/************************/
/* HEADER STATE        */
/************************/
<HEADER>     
        {
			{COMPONENT}{ELEM_DESC}		{hasComponent = true;}
            {FILE}{ELEM_DESC}         {hasFile = true;}
            {AUTHOR}{ELEM_DESC}        {hasAuthor = true;}
            {COPYRIGHT}{ELEM_DESC}     {hasCopyright = true;}
            {DESCRIPTION}{ELEM_DESC}   {hasDescription = true;}
            \n              {yybegin(NEW_LINE);}
            .               {}
        }
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>    
        {
            {COMMENT_WORD}      {if(startProgMod == true) yybegin(HEADER); 
								 else yybegin(COMMENT);}
            {ENDHEADER}         {if(startProgMod == true) checkFileHeader();}
            {END}               {yybegin(AVOID);}
			{SPACE}				{}
            {MODPROG}           {location=yytext(); line=yyline+1; startProgMod = true; yybegin(NEW_LINE);}
            \n                  {yybegin(NEW_LINE);}
            .                   {yybegin(LINE);}
        }
/************************/
/* LINE STATE           */
/************************/
<LINE>            
        {
            {COMMENT_WORD}      {if(startProgMod == true) yybegin(HEADER); 
								 else yybegin(COMMENT);}
            {ENDHEADER}         {if(startProgMod == true) checkFileHeader();}
            {END}               {yybegin(AVOID);}
            \n                  {yybegin(NEW_LINE);}
            .                   {yybegin(LINE);}
        }
/************************/
/* ERROR STATE          */
/************************/
                [^]            {
                                    
                                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
                                    throw new JFlexException(this.getClass().getName(), parsedFileName, errorMessage, yytext(), yyline, yycolumn);
                                }