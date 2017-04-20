/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.NotUsed  rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATANotUsed
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, STRING

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

																
%{
	String location = "MAIN PROGRAM";
	List<String> variables = new ArrayList<String>();
	List<String> allVariables = new ArrayList<String>();
	List<Integer> lines = new ArrayList<Integer>();
	List<String> locations = new ArrayList<String>();

    public COMDATANotUsed() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	/**
	  * Save the new variable into the list
	  **/
	private void saveVariable (String variable) {
		variable = variable.split("=")[0];
		variables.add(variable);
		lines.add(yyline+1);
		locations.add(location);
	}
	
	/**
	  * Throw error when a variable in 'variables' list is not in 
	  * 'allVariables' list
	  **/
	private void notUsedVariables()  throws JFlexException {
		for (int i = 0; i < variables.size(); i++) {
			String var = variables.get(i);
			if (!allVariables.contains(var)) {
				setError(locations.get(i),"The variable $" + var + " is declared and not used.", lines.get(i));
			}
		}
	}
	
	
		
%}

%eofval{
	notUsedVariables();
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
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {VAR}\=			{saveVariable(yytext());}
			    \$(\{)?{VAR}	{String var = yytext().replaceAll("\\$","").replaceAll("\\{","");
			    				 allVariables.add(var);}
			    \"				{yybegin(STRING);}
			 	.              	{}
		}

/************************/
/* STRING STATE	    */
/************************/
<STRING>   	
		{
				\$(\{)?{VAR}	{String var = yytext().replaceAll("\\$","").replaceAll("\\{","");
			    				 allVariables.add(var);}
				\"				{yybegin(YYINITIAL);}
			   	.              	{}
		}



/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}