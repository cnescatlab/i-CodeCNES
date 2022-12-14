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

%class COMINSTBrace
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE,AVOID, FUNC

COMMENT_WORD = \! 
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
AVOID		 = "COMMON"	  | "SAVE"	   | "DATA"		| \#
VAR		     = [a-zA-Z][a-zA-Z0-9\_]* 
INT			 = [0-9]+  ( \. [0-9]+ )?  ( ("D" | "E")(\+ | \-)[0-9]+ )?
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t\f]
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<Integer> parenthesis = new LinkedList<Integer>();
	List<Integer> operators   = new LinkedList<Integer>();
	boolean end = true;
	
	public COMINSTBrace(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	
	

	private void addParenthesis(){
		parenthesis.add(1);
		operators.add(0);
	}

	private void closeParenthesis() throws JFlexException {
		int index = parenthesis.size() - 1;
		int value = parenthesis.get(index) - 1;
		if(index<0){
			throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis couldn't handle parenthesis closure.", yytext(), yyline, yycolumn);
		}
		parenthesis.remove(index);
		parenthesis.add(value);
		if (value == 0) {
			if (operators.get(index) > 1) setError(location,"Parentheses are needed for readability.", yyline+1);
			parenthesis.remove(index);
			operators.remove(index);
		}
	}
	
	private void addOperator() {
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
	}
	
	private void checkOperators() throws JFlexException {
		if(!operators.isEmpty()) {
			int index = operators.size() - 1;
			if (operators.get(index) > 1) {
				setError(location,"Parentheses are needed for readability.", yyline+1);
			}
			parenthesis.clear();
			operators.clear();
		}
	}
	
	private void parameterFunction() throws JFlexException {
		if(!parenthesis.isEmpty()) {
			int index = operators.size() - 1;
			int value = operators.get(index);
			if(value > 1) setError(location,"Parentheses are needed for readability.", yyline+1);
			operators.remove(index);
			operators.add(0);
		}
	}
	
%}

%eofval{
    
	
	return getCheckResults();
%eofval}
%eofclose

%%          

/************************/

				{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(LINE);}
<NAMING>		\(				{yybegin(AVOID);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
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
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{AVOID}			{yybegin(AVOID);}
<NEW_LINE>		\/\/			{}
<NEW_LINE>		{OPERATOR}		{addOperator();}
<NEW_LINE>		{VAR}|{INT}		{}
<NEW_LINE>		\(				{addParenthesis();}
<NEW_LINE>		\)				{closeParenthesis();}
<NEW_LINE>		\,				{parameterFunction();}
<NEW_LINE>		&				{end=false;}
<NEW_LINE>     	\n             	{if(end) checkOperators();}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>     	.              	{end=true;}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{STRING}		{}
<LINE>			{AVOID}			{yybegin(AVOID);}
<LINE>			\/\/			{}
<LINE>			{OPERATOR}		{addOperator();}
<LINE>			{VAR}|{INT}		{}
<LINE>			\(				{addParenthesis();}
<LINE>			\)				{closeParenthesis();}
<LINE>			\,				{parameterFunction();}
<LINE>			&				{end=false;}
<LINE>      	\n             	{if(end) checkOperators(); yybegin(NEW_LINE);}
<LINE>			{SPACE}			{}
<LINE>      	.              	{end=true;}


/************************/	
/* AVOID STATE    	    */
/************************/
<AVOID>			{VAR}			{end=true;}
<AVOID>			\&	        	{end=false;}
<AVOID>			\n				{if(end) yybegin(NEW_LINE);}
<AVOID>			.				{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }