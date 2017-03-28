/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DESIGN.Interface rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DESIGNInterface
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT,COMMENT_MODULE, NAMING, MODULE, MODULE_DEF,INTERFACE


COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" | "interface"
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \t\f]

/** All the keyword except PRIVATE, PUBLIC, USE and IMPLICIT NONE are considered errors **/
ERROR		 = "allocatable" | "allocate" | "assign" | "backspace" | "block" | "call" | 
			   "case" | "close" | "common" | "contains" | "continue" | "cycle" | "data" | 
			   "deallocate" | "do" | "else" | "elsewhere" | "endfile" | "entry" | 
			   "equivalence" | "exit" | "external" |"format" | "function" | "goto" | "if" | 
			   "include" | "inquire" | "intent" | "intrinsic" | "namelist" | "nullify" | 
			   "open" | "operator" | "optional" | "pause" | "print" | "procedure" | "program" | 
			   "read" | "recursive" | "result" | "return" | "rewind" | "rewrite" | "save" | 
			   "select" | "sequence" | "stop" | "subroutine" | "target" | "then" |"where" | 
			   "while" | "write"

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variables to store the error in module definition line**/
	boolean inter = false;
	boolean error = false;
	
	public F90DESIGNInterface() {
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
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n|\r           {yybegin(YYINITIAL);}  
			.              	{}
		}
		
/************************/
/* COMMENT_MODULE STATE */
/************************/
<COMMENT_MODULE>   	
		{
			\n|\r           {yybegin(MODULE);}  
			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>
		{
				{COMMENT_WORD}	{yybegin(COMMENT);}
				{VAR}			{if (!(location.toLowerCase().equals("module") && yytext().toLowerCase().contains("interface"))) {
								 	location = location + " " + yytext(); 
							 	 }
							 	 yybegin(COMMENT);}
    			\n          	{yybegin(YYINITIAL);}
    			.          		{}
    	}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>
		{
		  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{}
			"module"		{location = yytext(); yybegin(MODULE_DEF);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
		 	\n             	{}
		 	.              	{}
		 }

/************************/
/* MODULE_DEF STATE     */
/************************/
<MODULE_DEF>
		{
			{VAR}			{location = location + " " + yytext();}
		 	\n             	{error=false; yybegin(MODULE);}
		 	.              	{}
		 }
		 
/************************/
/* MODULE STATE 	    */
/************************/
<MODULE>
		{
			{COMMENT_WORD} 			{yybegin(COMMENT_MODULE);}
			{STRING}				{}
			"interface"				{inter=true; yybegin(INTERFACE);}
			{ERROR}					{if(inter&&!error) { setError(location,"Interface Module shall only contain: INTERFACE, USE, IMPLICIT instructions as well as PRIVATE or PUBLIC declaration.", yyline+1);
									 error=true;}}
			"end"{SPACE}*"module"	{inter=false; error=false; yybegin(YYINITIAL);}
			{VAR}					{}
		 	\n             			{}
		 	.              			{}
		 }
		 
/************************/
/* INTERFACE STATE 	    */
/************************/
<INTERFACE>
		{
			"end"{SPACE}*"interface"	{yybegin(MODULE);}
			{VAR}						{}
		 	\n             				{}
		 	.              				{}
		 }


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
