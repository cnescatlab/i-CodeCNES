/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.Variable rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90REFVariable
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT, NAMING, NEW_LINE, LINE, CALLING, CALLING_PARAM, FUNCTION_PARAM

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

DATA_TYPE	 = "integer" | "real" | "logical" | "character" | "complex	" | ("double"){SPACE}*("precision") 
CALL		 = "call"
CLE			 = "assign" | "backspace" | ("block"){SPACE}*("data") | "close" | "common" | "continue" | "data" | 
			   "dimension" | "do" | "else" | "else"{SPACE}*"if" | "end" | "end"{SPACE}*"file" | "end"{SPACE}*"if" | "entry"| "equivalence" | 
			   "external" | "format" | "goto" | "if" | "implicit" | "inquire" | "intrinsic" | "open" | 
			   "parameter" | "pause" | "print" | "read" | "return" | "rewind" | "rewrite" | "save" | 
			   "stop" | "then" | "write" | "allocate" | "allocatable" | "case" | "contains" | 
			   "cycle" | "deallocate" | "elsewhere" | "exit" | "include" | "interface" | "intent" |  
			   "namelist" | "nullify" | "only" | "operator" | "optional" | "pointer" | "private" |  
			   "public" | "result" | "recursive" | "select" | "sequence" | "target" | "use" | "while" | "where" |
			   {CALL}
PARAM		 = \( [^\)]* \)

%{
	String location = "MAIN PROGRAM"; 
	 List<Violation> list = new LinkedList<Violation>();
	String functionName;
	Map<String, List<String>> funcCalls = new HashMap<String, List<String>>();
	Map<String, List<String>> funcDecls = new HashMap<String, List<String>>();
	
	public F90REFVariable() {
    }
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	
	
	private void insertParams (String word) {
		String[] params = word.substring(1, word.length()-1).replaceAll("[\\t\\n\\r\\&]+","").split("\\,");
		List<String> list = new LinkedList<String>();
		for (int i = 0; i < params.length; i++) {
			list.add(params[i].trim());
		}
		funcCalls.put(functionName, list);
	}
	
	private void insertParamsFunction (String word) {
		String[] params = word.substring(1, word.length()-1).replaceAll("[\\t\\n\\r\\&]+","").split("\\,");
		List<String> list = new LinkedList<String>();
		for (int i = 0; i < params.length; i++) {
			list.add(params[i].trim());
		}
		funcDecls.put(functionName, list);
	}
	
	private void checkVar(String variable) throws JFlexException {
		if(location.split(" ").length > 1) {
			String loc = location.split(" ")[1];
			List<String> listD = funcDecls.get(loc);
			List<String> listC = funcCalls.get(loc);
			if(listD != null && listC != null) {
				if (!listD.contains(variable) && listC.contains(variable)) {
					setError(location,"The variable "+variable+" is used with different names inside the subprogram.", yyline+1);
				}
			}
		}
	}
	
%}

%eofval{ 
  
 return getViolations(); 
%eofval}


%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE         */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext();
								 functionName = yytext();	
								 yybegin(FUNCTION_PARAM);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{CALL}			{yybegin(CALLING);}
<NEW_LINE>		{CLE} 			{}
<NEW_LINE>		{DATA_TYPE}		{yybegin(COMMENT);}
<NEW_LINE>		{VAR}			{checkVar(yytext());}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{CALL}			{yybegin(CALLING);}
<LINE>			{CLE}			{}
<LINE>			{DATA_TYPE}		{yybegin(COMMENT);}
<LINE>			{VAR}			{checkVar(yytext());}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* CALLING STATE     	*/
/************************/
<CALLING>	 	{VAR}         	{functionName = yytext(); yybegin(CALLING_PARAM);}
<CALLING>   	\n             	{yybegin(NEW_LINE);}
<CALLING>   	.              	{}


/************************/
/* CALLING_PARAM STATE  */
/************************/
<CALLING_PARAM>	{PARAM}        	{insertParams(yytext());}
<CALLING_PARAM>	\n             	{yybegin(NEW_LINE);}
<CALLING_PARAM>	.              	{}


/************************/
/* FUNCTION_PARAMS STATE*/
/************************/
<FUNCTION_PARAM>	{PARAM}        	{insertParamsFunction(yytext());}
<FUNCTION_PARAM>	\n             	{yybegin(NEW_LINE);}
<FUNCTION_PARAM>	.              	{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
