/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.INST.Associated rule.*/
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90INSTAssociated
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* 3 states are added :											*/
/*    - POINTER_DEC, to get all variables declared as a pointer	*/
/*	  - NULL_STATE, to get all pointers that are nullified		*/
/*	  - ASS_STATE, to test association check on a pointer could	*/
/*		raise a violation										*/
%state COMMENT, NAMING, POINTER_DEC, NULL_STATE, ASS_STATE

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
POINTERVAR   = {VAR}(\%{VAR})*
POINTERVARPAR= \( {SPACE}* {POINTERVAR} {SPACE}* \) 
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* 4 words are declared : 										*/
/*    - POINTER, to deal with pointer declaration				*/
/*    - INIT, to deal with pointer association to a variable	*/
/*    - NULLIFY, to catch 'nullify' word						*/
/*    - ASSOCIATED, to deal with variables association testing	*/
POINTER		 = ("pointer"){SPACE}*\:\:
INIT		 = {VAR}{SPACE}*("=>")
NULLIFY		 = ("nullify")
ASSOCIATED	 = ("associated")

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	/** List of pointer name and associated boolean, which is true when pointer is assigned to a target. **/
	Map<String, Boolean> pointers = new HashMap<String, Boolean>();
	/** String to contain pointer's name. **/
	String pointerVar = "";
	
	public F90INSTAssociated() {
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
			{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n|\r           {yybegin(YYINITIAL);}  
			.              	{}
		}

/************************/
/* NAMING STATE        */
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n|\r           {yybegin(YYINITIAL);}
			.              	{}
		}

/****************************************************************/
/* YYINITAL STATE       										*/
/*																*/
/* 4 steps are done :											*/
/*    - if POINTER is met, we start POINTER_DEC state			*/
/*	  - if INIT is met, we erase spaces from catched word and	*/
/*		take the part before '=>'. This represents the pointer	*/
/*		name, which corresponding boolean is set to true		*/
/*	  - if NULLIFY is met, we start NULL_STATE state			*/
/*	  - if ASSOCIATED is met, we start ASS_STATE state			*/
/****************************************************************/
<YYINITIAL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}        	{location = yytext(); 
							 yybegin(NAMING);}
			{POINTER}		{yybegin(POINTER_DEC);}
			{INIT}			{pointers.put(yytext().replace(" ","").split("=")[0],true);}
			{NULLIFY}		{yybegin(NULL_STATE);}
			{ASSOCIATED}	{yybegin(ASS_STATE);}
			\n             	{}
			.              	{}
		}

/************************************************************/
/* POINTER_DEC STATE    									*/
/*															*/
/* If VAR is met, we store variable name in pointerVar and	*/
/* add the variable with default false value.				*/
/* IF '=>' is met, we set pointerVar associated boolean 	*/
/* value to true.											*/
/************************************************************/
<POINTER_DEC>	
		{
			"NULL"			{}
			{VAR}			{pointerVar = yytext();
							 pointers.put(yytext(), false);}
			"=>"			{pointers.put(pointerVar, true);}
			\n				{yybegin(YYINITIAL);}
			.				{}
		}

/****************************************************************/
/* NULL_STATE STATE     										*/
/*																*/
/* If VAR is met, we set variable associated boolean to false.	*/
/****************************************************************/
<NULL_STATE>	
		{
			{POINTERVAR}	{pointers.put(yytext(), true);}
			\n				{yybegin(YYINITIAL);}
			.				{}
		}

/****************************************************************/
/* POINTER_DEC STATE    										*/
/*																*/
/* If VAR is met, we get its corresponding boolean. If there	*/
/* is none or it is false, we throw an error. We then move to	*/
/* YYINITIAL. 													*/
/****************************************************************/
<ASS_STATE>		
		{
			{POINTERVARPAR}	{String var = yytext().replaceAll("\\(", "").replaceAll("\\)", "").trim();
							 Boolean value = pointers.get(var);
							 if(value!=null && !value) {
								setError(location,"The pointer "+yytext()+" is not set to null before the use of the instruction ASSOCIATED.", yyline+1); 
							 }
							}
			\n				{yybegin(YYINITIAL);}
			.				{}
		}

/************************/
/* THROW ERROR          */
/************************/
			[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
