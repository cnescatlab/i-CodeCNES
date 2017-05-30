/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.LoopCondition  rule. */
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

%class COMDATALoopCondition
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, WHILE, FOR

COMMENT_WORD = \#
FUNC         = "function"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
WHILE		 = "while"
FOR			 = "for"
DONE		 = "done"

																
%{
	String location = "MAIN PROGRAM";
	List<List<String>> conditions = new ArrayList<List<String>>();
	List<String> variables = new ArrayList<String>();

    public COMDATALoopCondition() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/**
	 * Find variable in the conditions list. 
	 * If it exist:
	 *   - there are more than one variable in the list: delete it
	 *   - is the only variable (means that its the last modification): error
	 * If not exist:
	 *   - the variable doesn't belong to loop condition variables: nothing
	 * 
	 * @params
	 *    var 
	 * @throws JFlexException 
	 **/
	private void checkVariable(String var) throws JFlexException {
		for (int i = 0; i < conditions.size(); i++) {
			if (conditions.get(i).contains(var)) {
				if (conditions.get(i).size() == 1) 
					this.setError(location,"The variable " + var + " is modified inside the loop.", yyline+1);
				else 
					conditions.get(i).remove(var);
			}
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
				{VAR}			{location = location + yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  	{yybegin(COMMENT);}
				{STRING}			{}
				{FUNC}        		{location = yytext(); yybegin(NAMING);}
			    {WHILE}				{yybegin(WHILE);}
			    {FOR}				{yybegin(FOR);}
			    {VAR}\=				{String var = yytext().substring(0, yytext().length()-1); checkVariable(var);}
			    {DONE}				{int index = conditions.size() - 1; if (index >= 0) {conditions.remove(index);}}
			    {VAR}				{}
			 	.              		{}
		}
		
/************************/
/* WHILE STATE	        */
/************************/
<WHILE>
		{
				/** Save the loop conditions variables **/
				\${VAR}				{String var = yytext().substring(1); 
									 if (!variables.contains(var)) variables.add(var);}
			    {VAR} 				{}
			    /** End while condition: save variables **/
			    \n | \;				{List<String> variableslist = new ArrayList<String>(variables);
			    					 conditions.add(variableslist);
			    					 variables.clear(); yybegin(YYINITIAL);}
			 	.              		{}
		}
		
/************************/
/* FOR STATE	   		*/
/************************/
<FOR>   	
		{
				\n  | \;       	{List<String> variableslist = new ArrayList<String>();
			    				 conditions.add(variableslist);
			    				 yybegin(YYINITIAL);}  
			   	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}