/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Brace rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMINSTBrace
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, BRACE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

ARIT		 = 	\+		| \-		| \*		| \/		|
			   	\*\*	| \%		| \+\=		| \-\=		|
				\*\=	| \/\=		| \%\=
BIT			 =  \<\<	| \<\<\=	| \>\>		| \>\>\=	|
				\&		| \&\=		| \|		| \|\=		|
				\~		| \^		| \^\=
LOG			 =  \!		| \&\&		| \|\|
OPER		 = {ARIT}	| {LOG}		| {BIT}  	

BRACING		 = "expr"	| "let"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<Integer> parenthesis = new LinkedList<Integer>();
	List<Integer> operators   = new LinkedList<Integer>();

    public COMINSTBrace() {
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
		if (index >= 0) {
			int value = parenthesis.get(index) - 1;
			parenthesis.remove(index);
			parenthesis.add(value);
			if (value == 0) {
				if (operators.get(index) > 1) setError(location,"The parenthesis of this expression are not proper.", yyline+1);
				parenthesis.remove(index);
				operators.remove(index);
			}
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
				setError(location,"The parenthesis of this expression are not proper.", yyline+1);
			}
			parenthesis.clear();
			operators.clear();
		}
	}
	
	private void parameterFunction() throws JFlexException {
		if(!parenthesis.isEmpty()) {
			int index = operators.size() - 1;
			int value = operators.get(index);
			if(value > 1) setError(location,"The parenthesis of this expression are not proper.", yyline+1);
			operators.remove(index);
			operators.add(0);
		}
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
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext().substring(0,yytext().length()-2).trim(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.            	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
				{COMMENT_WORD}	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();}
			    "export"		{yybegin(COMMENT);}
				{BRACING}		{yybegin(BRACE);}
				{VAR}			{}
	      		[^]          	{}
		}
		
/************************/
/* BRACE STATE	    	*/
/************************/
<BRACE>   	
		{
				
				{OPER}				{addOperator();}
				\(					{addParenthesis();}
				\)					{closeParenthesis();}
				\,					{parameterFunction();}
    		  	\n | \; | \`   		{checkOperators(); yybegin(YYINITIAL);}
	      		[^]            		{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}