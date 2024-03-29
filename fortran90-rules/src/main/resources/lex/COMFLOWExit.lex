/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Exit rule. 		*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

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

%class COMFLOWExit
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTER		 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}	| {INTER}
RETURN		 = "RETURN"
END			 = "END"
END_TYPE	 = {END} [\ ]+ {TYPE}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> loc = new LinkedList<String>();
	boolean returnExist = false;
	
	public COMFLOWExit(){
		loc.add(location);
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
%}

%eofval{
	return getCheckResults();
%eofval}
%eofclose

%%          

/************************/

				{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); returnExist = false; loc.add(location); yybegin(COMMENT);}
<NAMING>    	\n             	{loc.add(location); yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{END_TYPE}		{
									if(loc.isEmpty()){
										
					                    final String errorMessage = "Analysis failure : Location unreachable.";
					                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
							 	 	}
									loc.remove(loc.size()-1);
								}
<NEW_LINE>		{RETURN}		{
								 if(loc.isEmpty()){
								 		
					                    final String errorMessage = "Analysis failure : Location unreachable.";
					                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
							 	 }
								 if(returnExist){
								 	setError(loc.get(loc.size()-1),"There is more than one exit in the function.", yyline+1);
								 }else{
								 	returnExist = true;
								 }
								}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location=yytext(); yybegin(NAMING);}
<LINE>			{END_TYPE}		{
									if(loc.isEmpty()){
										
					                    final String errorMessage = "Analysis failure : Location unreachable.";
					                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
							 	 	}
									loc.remove(loc.size()-1);
								}
<LINE>			{RETURN}		{
								 if(loc.isEmpty()){
								 		
					                    final String errorMessage = "Analysis failure : Location unreachable.";
					                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
							 	 }
								 if(returnExist){ 
								 	setError(loc.get(loc.size()-1),"There is more than one exit in the function.", yyline+1);
								 }else{ 
								 	returnExist = true;
								 }
								}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }