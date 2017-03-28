/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Recursion rule.  	  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWRecursion
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, FUNCOMMENT, NAMING, FUNCTIONSTATE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_\-]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
OPTCHAR		 = \# | \! | % | \* | @ | \^ | \' | , | \/ | : | = | \+ | \?
EXTENDEDVAR	 = \$\{ {OPTCHAR}* {VAR} {OPTCHAR}* {VAR}? (\[)? {OPTCHAR}* (\])? \}
IGNORE		 = "EOF" [^]* "EOF"


																
%{
	String location = "MAIN PROGRAM";
	/** Map that contains all the functions with its calls **/
	Map<String,List<String>> functionCalls = new HashMap<String,List<String>>();
	/** Current list of calls **/
	List<String> calls = new ArrayList<String>();
	/** Number of brackets **/
	int brackets = 0;

    public COMFLOWRecursion() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	/** 
	  * This function checks if there are circular function calling using the 
	  * lost 'functionCalling' and throw an error when it finds one
	  *
	  * @param
	  *     var: string called inside the current function 
	  */
	private void checkCircularCalling(String var) throws JFlexException {
		List<String> callings = functionCalls.get(var);
		if (callings!=null) {
			if (callings.contains(location)) setError(location,"The use of recursivity is not allowed.", yyline+1);
			else {
				for (String element : callings) checkCircularCalling(element);
			}
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
/* FUNCOMMENT STATE	    */
/************************/
<FUNCOMMENT>   	
		{
				\n             	{yybegin(FUNCTIONSTATE);}  
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); brackets=0; yybegin(FUNCTIONSTATE);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); brackets=0; yybegin(FUNCTIONSTATE);}
			    {STRING}		{}
			 	.              	{}
		} 
		
/************************/
/* FUNCTIONSTATE STATE	*/
/************************/
<FUNCTIONSTATE>   	
		{
				{COMMENT_WORD}	{yybegin(FUNCOMMENT);}
				{STRING}		{}
				{EXTENDEDVAR}	{}
				{VAR}			{/** call to the same function **/
								 if(yytext().equals(location)) setError(location,"The use of recursivity is not allowed.", yyline+1);
								 /** save in list to verify circular calling **/
								 else {
								 	calls.add(yytext());
								 	checkCircularCalling(yytext());
								 }}
				\{(\#)?			{brackets++;}
				\}				{brackets--;
								 if(brackets==0) {
								 	List<String> list = new ArrayList<String>(calls);
			    					functionCalls.put(location,list);
			    					calls.clear();
								 	yybegin(YYINITIAL);}}
				{IGNORE}		{}
			   	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}