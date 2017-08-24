/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.TYPE.Derivate rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90TYPEDerivate
%extends AbstractChecker
%public
%line
%column
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
END_TYPE	 = "end"{SPACE}*{TYPE} 
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

STRUCT		 = "type"{SPACE}+{VAR}
END_STRUCT	 = "end"{SPACE}*"type"

%{
	String location = "MAIN PROGRAM"; 
     List<String> locations = new LinkedList<String>(); 
     private String parsedFileName;
	
	public F90TYPEDerivate() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
%}

%eofval{ 
	return getCheckResults(); 
%eofval}

%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); locations.add(location); yybegin(COMMENT);}
<NAMING>    	\n             	{locations.add(location); yybegin(NEW_LINE);}
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
<NEW_LINE>		{STRING}|{FALSE} {}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{END_TYPE}		{
								if(locations.isEmpty()){
									throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Location unreachable.", yytext(), yyline, yycolumn);
								}
									locations.remove(locations.size() - 1);}
<NEW_LINE>		{END_STRUCT}	{}
<NEW_LINE>		{STRUCT}		{
									if(locations.isEmpty()){
										throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Location unreachable.", yytext(), yyline, yycolumn);
									}
									if (!locations.get(locations.size()-1).toLowerCase().contains("module")) 
								 setError(location,"The " + yytext() + " must be defined inside the module structure." , yyline+1);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}|{FALSE} {}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{END_TYPE}		{
									if(locations.isEmpty()){
										throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Location unreachable.", yytext(), yyline, yycolumn);
									}
									locations.remove(locations.size() - 1);
								}
<LINE>			{END_STRUCT}	{}
<LINE>			{STRUCT}		{
									if(locations.isEmpty()){
										throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Location unreachable.", yytext(), yyline, yycolumn);
									}
									if (!locations.get(locations.size()-1).toLowerCase().contains("module")) 
								 setError(location,"The " + yytext() + " must be defined inside the module structure." , yyline+1);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
