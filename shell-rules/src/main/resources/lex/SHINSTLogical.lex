/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.Logical rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class SHINSTLogical
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, LOGICAL, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

LOGIC		 = \|\|		| \&\&
LOGICCORRECT = "echo"	| "exit"

COND		 = "if"		| "until"	| "for"		| "while"	| "elif"
ENDCOND		 = "do"		| "then"

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;

    public SHINSTLogical() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
			
%}

%eofval{
   	this.zzReader.close();
	return getCheckResults();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID>   	
		{
				{ENDCOND}		{yybegin(YYINITIAL);}
			   	[^]	        	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}		{}
			    {COND} 			{yybegin(AVOID);}
			    {LOGIC}			{yybegin(LOGICAL);}
			    {VAR}			{}
	      		[^]         	{}
		}
		
/************************/
/* LOGICAL STATE   		*/
/************************/
<LOGICAL>   	
		{
				{LOGICCORRECT}	{yybegin(YYINITIAL);}
				{VAR}			{setError(location,"The abbreviation || and && must be followed only by ECHO or EXIT.", yyline+1); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}
				{SPACE}			{}  
			   	.              	{setError(location,"The abbreviation || and && must be followed only by ECHO or EXIT.", yyline+1); yybegin(YYINITIAL);}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}