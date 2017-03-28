/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.CheckUser rule. 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWCheckUser
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, CHECK_USER, NAMING_PROGRAM

FREE_COMMENT = \!
TYPE		 = "function"  | "procedure" | "subroutine" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

GETUID		 = "GETUID"{SPACE}*\({SPACE}*\)
																
%{
	String location = "MAIN PROGRAM";
	boolean getuid = false;
	int line = 0;
	
	public COMFLOWCheckUser(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
		
%}

%eofval{
	return getViolations();
%eofval}


%%          

/************************/

			{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE	        */
/************************/
<NAMING>
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* NAMING_PROGRAM STATE	*/
/************************/
<NAMING_PROGRAM>
		{
			{VAR}			{location = "PROGRAM " + yytext(); 
							 yybegin(CHECK_USER);}
			\n             	{yybegin(CHECK_USER);}
			.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	
		{
			"program"		{line=yyline; yybegin(NAMING_PROGRAM);}
			{TYPE}        	{location = yytext(); 
							 yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			"program"		{line=yyline; yybegin(NAMING_PROGRAM);}
			{STRING}		{yybegin(LINE);}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			\n             	{}
			.              	{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			"program"		{line=yyline; yybegin(NAMING_PROGRAM);}
			{STRING}		{yybegin(LINE);}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}



/************************/
/* CHECK_USER STATE     */
/************************/
<CHECK_USER>			
		{
			{GETUID}				{getuid=true;}
			"end"{SPACE}*"program"	{if(!getuid) setError(location,"The user identity is not verified in the main program.", line+1); 
									 yybegin(LINE);}
			\n             			{}
			.              			{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}