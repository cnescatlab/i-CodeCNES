/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.BLOC.Function rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77BLOCFunction
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* Two states are added:
 * RIGHT_SIDE: state that show the right side after = symbol
 * PAR: if a variable in the code is a function the next character must be an open parenthesis
 */
%state COMMENT, NAMING, NEW_LINE, LINE, RIGHT_SIDE, PAR, FUNCTION

COMMENT_WORD = \!         | c          | C		| \* 
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
WORD		 = \'[^\']*\' | \"[^\"]*\"
PARENTHESIS  = [\ ]*\(
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
TYPE		 = {PROC}	   | {SUB} | {PROG} | {MOD}
EQUAL		 = \=

																
%{
    private static final Logger LOGGER = Logger.getLogger(F77BLOCFunction.class.getName());
	String location = "MAIN PROGRAM";
    String parsedFileName;
	
	List<String> functionList = new LinkedList<String>();
	List<String> functionCalls = new LinkedList<String>();
	List<String> errorLoc = new LinkedList<String>();
	List<Integer> errorLine = new LinkedList<Integer>();
	List<Boolean> parenthesis = new LinkedList<Boolean>();
	
	public F77BLOCFunction() {
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	private void checkList() throws JFlexException {
        LOGGER.finest("begin method checkList");
		for (int i = 0; i < functionCalls.size(); i++) {
			if (functionList.contains(functionCalls.get(i))) {
				if(!parenthesis.get(i)) {
					LOGGER.fine("Setting error line "+(errorLine.get(i))+" because the brackets following the function name are mandatory.");
					setError(errorLoc.get(i),"When calling a function, the brackets following the function name are mandatory.", errorLine.get(i));
				}
			}
		}
		functionList.clear();
		functionCalls.clear();
		errorLoc.clear();
		errorLine.clear();
		parenthesis.clear();
        LOGGER.finest("end method checkList");
	}

%}

%eofval{
	checkList();
return getCheckResults();
%eofval}

%%          
				{FREE_COMMENT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
                       				yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/************************/
/* NAMING STATE         */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
    							    yybegin(COMMENT);}
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>    	.              	{}

/************************/
/* YYINITIAL STATE      */
/************************/  
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{FUNC}			{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> FUNCTION (Transition : FUNC \""+yytext()+"\" )");
								    yybegin(FUNCTION);}
<YYINITIAL>  	{TYPE}         	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{FUNC}			{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> FUNCTION (Transition : FUNC \""+yytext()+"\" )");
                                    yybegin(FUNCTION);}
<NEW_LINE>  	{TYPE}         	{location = yytext();
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}

/************************/
/* LINE STATE           */
/************************/
<LINE>			{FUNC}			{location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> FUNCTION (Transition : FUNC \""+yytext()+"\" )");
								    yybegin(FUNCTION);}
<LINE>  		{TYPE}         	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
    								 yybegin(NAMING);}
<LINE>			{WORD}			{}
<LINE>			{EQUAL}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> RIGHT_SIDE (Transition : EQUAL \""+yytext()+"\" )");
                                    yybegin(RIGHT_SIDE);}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<LINE>      	.              	{}
/*****************************************************/
/* FUNCTION state save the function's name  in a List*/
/*****************************************************/
<FUNCTION>		{VAR}			{location = location + " " + yytext(); 
                                 functionList.add(yytext()); 
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNCTION -> LINE (Transition : VAR \""+yytext()+"\" )");
								 yybegin(LINE);}
<FUNCTION>		\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNCTION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<FUNCTION>		.				{}
/***********************************************************************************/
/* In the state FUNCION_NAME we must save the name of the function in the linkedist*/
/***********************************************************************************/
<RIGHT_SIDE>	{VAR}			{functionCalls.add(yytext()); 
								 errorLoc.add(location);
								 errorLine.add(yyline+1);
                                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - RIGHT_SIDE -> PAR (Transition : VAR \""+yytext()+"\" )");
								 yybegin(PAR);}
<RIGHT_SIDE>	\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - RIGHT_SIDE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<RIGHT_SIDE>	.				{}
/*********************************************************/
/* State PAR means that a function is called in the code */
/*********************************************************/
<PAR>			{PARENTHESIS}	{parenthesis.add(true); 
                                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PAR -> COMMENT (Transition : PARENTHESIS \""+yytext()+"\" )");
                                 yybegin(COMMENT);
                                 }
<PAR>			\n				{parenthesis.add(false); 
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PAR -> NEW_LINE (Transition : \\n )");
								 yybegin(NEW_LINE);}
<PAR>			.				{parenthesis.add(false); 
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PAR -> COMMENT (Transition : . )");
								 yybegin(COMMENT);}

				[^]           {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                               }