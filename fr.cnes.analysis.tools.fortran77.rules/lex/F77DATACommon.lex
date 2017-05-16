/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Tr.Parametres rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F77DATACommon
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, INCLUDE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
COMMON		 = COMMON     | common
INCLUDE		 = INCLUDE    | include 
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

																
%{
	private static final Logger LOGGER = Logger.getLogger(F77DATACommon.class.getName());
	String location = "MAIN PROGRAM";
	
	List<String> includeList    = new ArrayList<String>();
	List<String> commonList     = new ArrayList<String>();
	List<String> errorLocList   = new ArrayList<String>();
	List<Integer> errorLineList = new ArrayList<Integer>();
	
	String parsedFileName;
	
	public F77DATACommon() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		LOGGER.finest("begin method setInputFile");
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
		LOGGER.finest("end method setInputFile");
	}
	
	private void addCommon() {
		LOGGER.finest("begin method addCommon");
        commonList.add(getViolation().getFile().getName());
        errorLocList.add(this.location);
        errorLineList.add(this.yyline + 1);
        yybegin(LINE);
        LOGGER.finest("end method addCommon");
    }
	
	private void addInclude(String file) {
		LOGGER.finest("begin method addInclude");
		if (!includeList.contains(file))
			includeList.add(file);
		yybegin(LINE);
		LOGGER.finest("end method addInclude");
	}
	
	private void compareLists() throws JFlexException{
		LOGGER.finest("begin method compareLists");
		for (int i = 0; i < commonList.size(); i++) {
			if (!includeList.contains(commonList.get(i))) {
				setError(errorLocList.get(i),"The INCLUDE instruction shall be used to reference the needed common bloc.", errorLineList.get(i));
			}
		}
		LOGGER.finest("end method compareLists");
	}

	
%}

%eofval{
	compareLists();
	
	includeList.clear();
	commonList.clear();
	errorLocList.clear();
	errorLineList.clear();
	
return getViolations();
%eofval}


%%          

				{FREE_COMMENT}	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\")");
									yybegin(COMMENT);
								}

<COMMENT>   	\n             	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);
								}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\")");
									location = location + " " + yytext();
								 	yybegin(COMMENT);}
<NAMING>    	\n             	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : \\n )");
									yybegin(NEW_LINE);}
<NAMING>    	.              	{}

<YYINITIAL>  	{COMMENT_WORD} 	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : VAR \""+yytext()+"\")");
									yybegin(COMMENT);
								}
<YYINITIAL>		{STRING}		{}
<YYINITIAL>		{TYPE}        	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\")");
									location = yytext(); 
									yybegin(NAMING);
								}
<YYINITIAL>		{COMMON}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : COMMON \""+yytext()+"\")");
									addCommon();
								}
<YYINITIAL>		{INCLUDE}		{ 	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> INCLUDE (Transition : INCLUDE \""+yytext()+"\")");
									yybegin(INCLUDE);
								}
<YYINITIAL> 	\n             	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition :  \\n )");
									yybegin(NEW_LINE);
								}
<YYINITIAL> 	.              	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
									yybegin(LINE);
								}

<NEW_LINE>  	{COMMENT_WORD} 	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\")");
									yybegin(COMMENT);
								}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\")");
									location = yytext(); 
									yybegin(NAMING);
								}
<NEW_LINE>		{INCLUDE}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> INCLUDE (Transition : INCLUDE \""+yytext()+"\")");
									yybegin(INCLUDE);
								}
<NEW_LINE>		{COMMON}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : COMMON \""+yytext()+"\")");
									addCommon();
								}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
									yybegin(LINE);
								}

<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\")");
									location = yytext(); yybegin(NAMING);
								}
<LINE>			{INCLUDE}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> INCLUDE (Transition : INCLUDE \""+yytext()+"\")");
									yybegin(INCLUDE);
								}
<LINE>			{COMMON}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> LINE (Transition : COMMON \""+yytext()+"\")");
									addCommon();
								}
<LINE>      	\n             	{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition :  \\n )");
									yybegin(NEW_LINE);
								}
<LINE>      	.              	{}

<INCLUDE>		{STRING}		{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INCLUDE -> LINE (Transition : TYPE \""+yytext()+"\")");
									addInclude(yytext());
								}
<INCLUDE>		\n				{	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INCLUDE -> NEW_LINE (Transition :  \\n )");
									yybegin(NEW_LINE);
								}
<INCLUDE>		.				{}

				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}