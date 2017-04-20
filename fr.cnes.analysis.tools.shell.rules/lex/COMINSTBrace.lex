/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Brace rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTBrace
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, FUNCTION, BRACE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
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
	List<Integer> parenthesis = new LinkedList<Integer>();
	List<Integer> operators   = new LinkedList<Integer>();

    public COMINSTBrace() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
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
	return getViolations();
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
				{VAR}			{location = yytext(); yybegin(FUNCTION);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
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
	      		.              	{}
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
	      		.              		{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}