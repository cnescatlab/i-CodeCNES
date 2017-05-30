/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.REF.Open rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;

import java.util.List;

import org.eclipse.core.runtime.Path;




import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77REFOpen
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state called OPEN helps to determine whenever an OPEN statement begins. */
%state COMMENT, NAMING, NEW_LINE, LINE, OPEN

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* 5 booleans called fileFound, scratchFound, statusFound, unknownFound and	*/
/* positionFound are set and respectively check if FILE, SCRATCH, STATUS, 	*/
/* UNKNOWN of POSITION words are found.										*/
%{
	String location = "MAIN PROGRAM"; 
 
	boolean fileFound = false;
	boolean scratchFound = false;
	boolean statusFound = false;
	boolean unknownFound = false;
	boolean positionFound = false;
	boolean multLines = false;
	
	public F77REFOpen() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	public void checkViolation(final boolean fileFound, final boolean scratchFound, final boolean statusFound, final boolean unknownFound, final boolean positionFound) throws JFlexException {
		boolean norFileNorScratch = !fileFound && !scratchFound;
		boolean hasViolation = unknownFound || !positionFound || !statusFound || norFileNorScratch ;
		if (hasViolation){
			this.setError(location,"The instruction OPEN shall be called with the parameters FILE, STATUS and POSITION.", yyline + 1);
		}
	}
%}

%eofval{ 
return getCheckResults();
%eofval}

/* Rule words are FILE, SCRATCH, STATUS, UNKNOWN and POSITION. 		 */
/* The main rule word is OPEN, to see when an OPEN statement starts. */
RULE_WORD = open	 | OPEN 
FILE      = file     | FILE
SCRATCH   = scratch  | SCRATCH
STATUS    = status   | STATUS
UNKNOWN   = unknown  | UNKNOWN
POSITION  = position | POSITION

%%          
				{FREE_COMMENT}	{yybegin(COMMENT);}
				{STRING}		{}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	.              	{}

/* We check which words appear and treat the result at the end of the line. */
<OPEN>			{FILE}			{fileFound = true;}
<OPEN>			{SCRATCH}		{scratchFound = true;}
<OPEN>			{STATUS}		{statusFound = true;}
<OPEN>			{UNKNOWN}		{unknownFound = true;}
<OPEN>			{POSITION}		{positionFound = true;}
<OPEN>			&				{multLines = true;}
<OPEN>			.				{}
<OPEN>			\n				{if (!multLines) {
								 	this.checkViolation(fileFound, scratchFound, statusFound, unknownFound, positionFound);
								 	fileFound = false;
								 	scratchFound = false;
								 	statusFound = false;
								 	unknownFound = false;
								 	positionFound = false;
								 	yybegin(NEW_LINE);
								 }
								 multLines = false;}

<YYINITIAL>		{COMMENT_WORD}	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{RULE_WORD}		{yybegin(OPEN);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>		{COMMENT_WORD}	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{RULE_WORD}		{yybegin(OPEN);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>		.				{yybegin(LINE);}
<NEW_LINE>  	\n             	{}


<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{RULE_WORD}		{yybegin(OPEN);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}