/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.Open rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;

import java.util.List;

import org.eclipse.core.runtime.Path;




import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F90REFOpen
%extends AbstractChecker
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state called OPEN helps to determine whenever an OPEN statement begins. */
%state COMMENT, NAMING, NEW_LINE, LINE, OPEN

COMMENT_WORD = "!"
COMMENT_LINE = ("!"|"#")[^\n]*\n
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* 5 booleans called fileFound, scratchFound, statusFound, unknownFound and	*/
/* positionFound are set and respectively check if FILE, SCRATCH, STATUS, 	*/
/* UNKNOWN of POSITION words are found.										*/
%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	/** Boolean to determine if FILE is found. **/
	boolean fileFound = false;
	/** Boolean to determine if 'SCRATCH' is found. **/
	boolean scratchFound = false;
	/** Boolean to determine if STATUS is found. **/
	boolean statusFound = false;
	/** Boolean to determine if 'UNKNOWN' is found. **/
	boolean unknownFound = false;
	/** Boolean to determine if POSITION is found. **/
	boolean positionFound = false;
	/** Boolean to determine if IOSTAT is found. **/
	boolean iostatFound = false;
	/** Boolean to determine if the line is finished (there is no continuation mark). **/
	boolean endLine = true;
	
	public F90REFOpen() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	/**
	 * This method sent an error if a violation is found. There are 5 possible cases :
	 *    - IOSTAT is not found
	 *    - POSITION is not found
	 *    - STATUS is not found
	 *    - 'UNKNOWN' is used has a status
	 *    - FILE and 'SCRATCH' are missing (we can have a least one of them)
	 * @param fileFound
	 *			true if FILE is found, false otherwise
	 * @param scratchFound
	 *			true if 'SCRATCH' is found, false otherwise
	 * @param statusFound
	 *			true if STATUS is found, false otherwise
	 * @param unknownFound
	 *			true if 'UNKNOWN' is found, false otherwise
	 * @param positionFound
	 *			true if POSITION is found, false otherwise
	 * @param iostatFound
	 *			true if IOSTAT is found, false otherwise
	 **/
    public void checkViolation(final boolean fileFound,
            final boolean scratchFound, final boolean statusFound,
            final boolean unknownFound, final boolean positionFound,
            final boolean iostatFound) throws JFlexException {
        final boolean norFileNorScratch = !fileFound && !scratchFound;
        final boolean hasViolation =
                unknownFound || !positionFound || !statusFound || !iostatFound
                        || norFileNorScratch;
        if (hasViolation) {
            this.setError(this.location,"It misses one or more parameters in OPEN instruction. Mandato-ry parameters are FILE, STATUS, IOSTAT, POSITION.", this.yyline + 1);
        }
    }
%}

%eofval{ 
	 
	return getCheckResults();
%eofval}

/* Rule words are FILE, SCRATCH, STATUS, UNKNOWN and POSITION. 		 */
/* The main rule word is OPEN, to see when an OPEN statement starts. */
RULE_WORD = "open" 
FILE      = "file"
SCRATCH   = "scratch"
STATUS    = "status" 
UNKNOWN   = "unknown"
POSITION  = "position" 
IOSTAT    = "iostat"

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

/************************************************************/
/* OPEN STATE												*/
/* 															*/
/* We check which words appear and treat the result at the	*/
/* end of the line. 										*/
/************************************************************/
<OPEN>			
		{
			{COMMENT_LINE}			{}
			{FILE}					{fileFound = true;}
			{SCRATCH}				{scratchFound = true;}
			{STATUS}				{statusFound = true;}
			{UNKNOWN}				{unknownFound = true;}
			{POSITION}				{positionFound = true;}
			{IOSTAT}				{iostatFound = true;}
			"&"{SPACE}*[^\n\r] 	{}
			"&"						{endLine = false;}
			.						{}
			\n						{if (endLine) {
										this.checkViolation(fileFound, scratchFound, statusFound, unknownFound, positionFound, iostatFound);
										fileFound = false;
										scratchFound = false;
										statusFound = false;
										unknownFound = false;
										positionFound = false;
										yybegin(YYINITIAL);
									 }
									 endLine = true;
									}
		}

/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>	
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			{RULE_WORD}		{yybegin(OPEN);}
			{VAR}			{}
			\n             	{}
			.              	{}
		}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
