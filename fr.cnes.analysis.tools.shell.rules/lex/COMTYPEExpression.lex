/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.TYPE.Expresion rule.	  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMTYPEExpression
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, DECLARATION, EXPRESSION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
INT			 = [0-9]+

INT_COMP	 = "-eq"	| "-ne"		| "-gt"		| "-ge"		|
			   "-lt"	| "-le"		| \<		| \>\=		|
			   \>		| \>\=
STR_COMP	 = \=		| \=\=		| \!\=		
OPER		 = {INT_COMP}			| {STR_COMP} 

VARIABLE	 = (\"?)\${VAR}(\")?(\})?
EXPR		 = {VARIABLE} {SPACE}+ {OPER} {SPACE}+ ({STRING}|{INT}|{VARIABLE})


																
%{
	String location = "MAIN PROGRAM";
	List<String> chars = new ArrayList<String>();
	List<String> ints  = new ArrayList<String>();
	String variableName = "";
	String type = "none";

    public COMTYPEExpression() {
    	
    }
    
    private void checkExpression(String expression) throws JFlexException {
    	String var1 = typeExpression(expression.split(" ")[0]);
    	String var2 = typeExpression(expression.split(" ")[2]);
    	if(!var1.equals("none") && !var2.equals("none") && !var1.equals(var2))
    		setError(location,"Type error in this expression. Mixed type " + var1 + " with " + var2, yyline+1);
    	
    }
     
    private String typeExpression(String var) {
    	if (var.contains("$")) {
    		var = var.replaceAll("\"", "").replaceAll("\\{", "").replaceAll("\\}", "").replaceAll("\\$", "");
    		if (ints.contains(var)) return "int";
    		else if (chars.contains(var)) return "char";
    		else return "none";
    	}
    	else if (var.contains("\"") || var.contains("\'")) return "char";
    	else if (isInteger(var,10)) return "int";
    	else return "none";
    }
    
	public static boolean isInteger(String s, int radix) {
	    if(s.isEmpty()) return false;
	    for(int i = 0; i < s.length(); i++) {
	        if(i == 0 && s.charAt(i) == '-') {
	            if(s.length() == 1) return false;
	            else continue;
	        }
	        if(Character.digit(s.charAt(i),radix) < 0) return false;
		}
	    return true;
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
			
%}

%eofval{
	return getCheckResults();
%eofval}


%%          



/************************************************************************************/
/* Saves the variables into this two list depending if its value is a char or ints.	*/
/* If inside an expression, the two variables from the different list are used,		*/
/* the rule is violated nd the program return an error.								*/
/************************************************************************************/		
	
/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
			
/************************/
/* NAMING STATE	    	*/
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
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
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
				{VAR}\=			{variableName=yytext().substring(0,yytext().length()-1); yybegin(DECLARATION);}
				{EXPR}			{checkExpression(yytext());}	
				"expr"			{yybegin(EXPRESSION);}		
				{VAR}			{}
			    \n				{}
	      		.              	{}
		}
		
/************************/
/* DECLARATION STATE	*/
/************************/
<DECLARATION>   	
		{
				{INT}			{ints.add(variableName);}
				{STRING}		{chars.add(variableName);}
				\${VAR}			{String var = yytext().substring(1);
								 if (ints.contains(var)) ints.add(variableName);
								 else if (chars.contains(var)) chars.add(variableName);}
				{SPACE}			{}
				[^]         	{yybegin(YYINITIAL);}  
		}
		
/************************/
/* EXPRESSION STATE 	*/
/************************/
<EXPRESSION>   	
		{
				\${VAR}			{String currentType = typeExpression(yytext());
								 if (!currentType.equals("none") && !type.equals("none") && !type.equals(currentType)) {
								 	setError(location,"Type error in this expression. Mixed type " + type + " with " + currentType, yyline+1);
								 	type="none"; yybegin(COMMENT);
								 }
								 if(!currentType.equals("none")) type=currentType;
								}
				{SPACE}			{}
				\`				{type="none"; yybegin(YYINITIAL);}
				[^]         	{}  
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}