/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.Exit rule.			  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWExit
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, FUNCTION

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

RETURN		 = "return"
																
%{
	String location = "MAIN PROGRAM";
	int brackets=0;
	int returns=0;
	int lineError=0;

    public COMFLOWExit() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
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
				{VAR}			{location = yytext(); returns=0; brackets=0; yybegin(FUNCTION);}
				\n             	{returns=0; yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); returns=0; brackets=0; yybegin(FUNCTION);}
			    {STRING}		{}
			    {VAR}			{} /* Clause to match with words that contains "return" */
			 	.              	{}
		}
		
/************************/
/*  FUNCTION STATE      */
/************************/
<FUNCTION>
		{
				{RETURN}		{returns++;
								 /** If there is one return: save error line**/
								 if (returns==1) lineError=yyline+1;
								 /** If there are two returns: print the two errors (the first one saved and current) **/
				                 else if (returns==2) { setError(location,"There is more than one exit in the function.", lineError); 
				                                        setError(location,"There is more than one exit in the function.", yyline+1); }
				                 /** if there are more than two returns: print current error **/
				                 else if (returns>2) setError(location,"There is more than one exit in the function.", yyline+1);}
				{VAR}			{}
				\{				{brackets++;}
				\} 				{brackets--;
								 /** End of the function **/
								 if(brackets==0) yybegin(YYINITIAL); }
				[^]			{}
		}


/************************/
/* ERROR STATE	        */
/************************/
			[^]            {}