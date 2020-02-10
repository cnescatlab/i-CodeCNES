/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Brace rule.		*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTBrace
%extends AbstractChecker
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, FUNC

COMMENT_WORD = \!         | c          | C     | \*
COMMENT_LINE = \! [^\n]*  | \n {COMMENT_WORD} [^\n]*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
ARIT		 = \*\*       | \*		   | \/    | \+		| \-
LOGIC	     = \.AND\.	  | \.OR\.	   | \.NEQV\.		| \.XOR\.	|
			   \.EQV\.	  | \.NOT\.
RELAT		 = \.LT\.	  | \.LE\.	   | \.EQ\.			|\.NE\.		|
			   \.GT\.	  | \.GE\.
OPERATOR     = {ARIT} 	  | {LOGIC}    | {RELAT}
AVOID		 = "COMMON"	  | "SAVE"	   | "DATA"		| "EQUIVALENCE"	| \#
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9][\ ]	| \~
DEC			 = [0-9]+  ( \. [0-9]+ )?  ( ("D" | "E")(\+ | \-)[0-9]+ )?
VAR		     = [a-zA-Z][a-zA-Z0-9\_]* 
INT			 = [0-9]+  ( \. ( [0-9]+ )? )?  ( ("D" | "E")(\+ | \-)[0-9]+ )?
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t\f]
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMINSTBrace.class.getName());

	String location = "MAIN PROGRAM";
	
	List<Integer> parenthesis = new LinkedList<Integer>();
	List<Integer> operators   = new LinkedList<Integer>();
	boolean end = true, check = true;
    String parsedFileName;
	
	public COMINSTBrace(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}

	private void addParenthesis(){
        LOGGER.finest("begin method addParenthesis");
		parenthesis.add(1);
		operators.add(0);
        LOGGER.finest("end method addParenthesis");
	}

	private void closeParenthesis() throws JFlexException {
        LOGGER.finest("begin method closeParenthesis");
		int index = parenthesis.size() - 1;
		if(index >= 0) {
			int value = parenthesis.get(index) - 1;
			parenthesis.remove(index);
			parenthesis.add(value);
			if (value == 0) {
				if(operators.size() > index){
					if (operators.get(index) > 1){
					    LOGGER.fine("Setting error line "+(yyline+1)+" because parentheses are needed for readability.");
					    setError(location,"Parentheses are needed for readability.", yyline+1);
				    }
					parenthesis.remove(index);
					operators.remove(index);
				}else{
					
                    final String errorMessage = "Analysis failure : Operator not reachable.";
                    throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
				}
			}
		}
        LOGGER.finest("end method closeParenthesis");
	}
	
	private void addOperator() {
        LOGGER.finest("begin method addOperator");
		int index = operators.size() - 1; 
		if (index >= 0){
			int value = operators.get(index) + 1;
			operators.remove(index);
			operators.add(value);
		}
		else {
			parenthesis.add(0);
			operators.add(1);
		}
        LOGGER.finest("end method addOperator");
	}
	
	private void checkOperators() throws JFlexException {
        LOGGER.finest("begin method checkOperators");
		if(!operators.isEmpty()) {
			int index = operators.size() - 1;
			if (index >= 0) {
				if (operators.get(index) > 1) {
					LOGGER.fine("Setting error line "+(yyline+1)+" because parentheses are needed for readability.");
					setError(location,"Parentheses are needed for readability.", yyline+1);
				}
				parenthesis.clear();
				operators.clear();
			}
		}
        LOGGER.finest("end method checkOperators");
	}
	
	private void parameterFunction() throws JFlexException {
        LOGGER.finest("begin method parameterFunction");
		if(!parenthesis.isEmpty()) {
			int index = operators.size() - 1;
			if (index >= 0) {
				int value = operators.get(index);
				if(value > 1){
				    LOGGER.fine("Setting error line "+(yyline+1)+" because parentheses are needed for readability.");
				    setError(location,"Parentheses are needed for readability.", yyline+1);
			    }
				operators.remove(index);
				operators.add(0);
			}
		}
        LOGGER.finest("end method parameterFunction");
	}
	
%}

%eofval{
    return getCheckResults();
%eofval}


%%          

/************************/


/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{COMMENT_LINE}	{
                                    if(end){
                                        checkOperators();
                                    }
                                }
<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> LINE (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{COMMENT_LINE}	{
                                    if(end){
                                        checkOperators();
                                    }
                                }
<NEW_LINE>		{TYPE}        	{
                                    checkOperators();
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		{AVOID}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : AVOID \""+yytext()+"\" )");
                                    yybegin(AVOID);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : AVOID \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		\/\/			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : // )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		{OPERATOR}		{
                                    addOperator();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : OPERATOR \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		{VAR}|{INT}		{}
<NEW_LINE>		\(				{
                                    addParenthesis();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : ( )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		\)				{
                                    closeParenthesis();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : ) )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		\,				{
                                    parameterFunction();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : , )");
                                    yybegin(LINE);
                                }
<NEW_LINE>		&				{
                                    end=false;
                                }
<NEW_LINE>     	\n             	{
                                    if(end){
                                        checkOperators();
                                    }
                                }
<NEW_LINE>		{SPACE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : SPACE \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>     	.              	{
                                    end=true;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{COMMENT_LINE}	{
                                    if(end){
                                        checkOperators();
                                    }
                                }
<LINE>			{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>			{STRING}		{}
<LINE>			{AVOID}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : AVOID \""+yytext()+"\" )");
                                    yybegin(AVOID);
                                }
<LINE>			{DEC}			{}
<LINE>			\/\/			{}
<LINE>			{OPERATOR}		{
                                    addOperator();
                                }
<LINE>			{VAR}|{INT}		{}
<LINE>			\(				{
                                    addParenthesis();
                                }
<LINE>			\)				{
                                    closeParenthesis();
                                }
<LINE>			\,				{
                                    parameterFunction();
                                }
<LINE>			&				{
                                    end=false;
                                }
<LINE>			\n[\ ]{1,5}{SIMBOL}	{}
<LINE>      	\n             	{
                                    if(end){
                                        checkOperators();
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{
                                    end=true;
                                }


/************************/	
/* AVOID STATE    	    */
/************************/
<AVOID>			{COMMENT_LINE}	{
    if(end){
        checkOperators();
    }
}
<AVOID>			{VAR}			{
                                  end=true;
                                }
<AVOID>			\&	        	{
                                  end=false;
                                }
<AVOID>			\n[\ ]{1,5}{SIMBOL}	{}
<AVOID>			\n				{
                                    if(end){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                        yybegin(NEW_LINE);
                                    }
                                }
<AVOID>			.				{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }
