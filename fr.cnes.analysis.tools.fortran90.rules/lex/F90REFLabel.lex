/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.REF.Label rule.		 */
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

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F90REFLabel
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, END_STATE, IF_STATE

COMMENT_LINE = ("!"|"#")[^\n]*
FUN_TYPE	 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface" | {BLOC}
FALSE        = [a-zA-Z0-9\_]({FUN_TYPE}) | ({FUN_TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({FUN_TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
INT			 = [0-9]+
END		 	 = ("end")
END_FUN_TYPE = {END}{FUN_TYPE}
BLOC 		 = ("block"){SPACE}*("data")
FALSE_END	 = ("end"[\ ]*"if") | ("end"[\ ]*"do") | ("end"[\ ]*"file") | ("end"[\ ]*"select") | ("end"[\ ]*"type")

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	/** Boolean to determine if statement is completed. **/
	Boolean endComplete = false;
	/** List of identifiers, to determine if an end is linked to a do, an if, a select of a function. **/
	List<String> identifiers = new LinkedList<String>();
	/** Boolean to determine if an IF statement if over. **/
	boolean endLine = true;
	
	
	public F90REFLabel() {
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

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n|\r           {yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE        */
/************************/
<NAMING>		
		{
			{COMMENT_LINE}	{}
			{VAR}			{location = location + " " + yytext().toLowerCase(); 
							 if (!location.contains("interface") && !location.contains("procedure"))
							 	identifiers.add(location);
							 yybegin(COMMENT);}
			\n|\r           {if(!location.contains("interface"))setError(this.location,"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
							 if (!location.contains("interface") && !location.contains("procedure"))
							 	identifiers.add(location);
							 yybegin(NEW_LINE);}
			.              	{}
		}


/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL> 
		{
			{COMMENT_LINE}	{}
			{STRING}		{}
			{FALSE}			{}
		  	{FUN_TYPE}      {location = yytext().toLowerCase(); yybegin(NAMING);}
			{VAR}			{}
			\n|\r          	{yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE> 
		{
			{COMMENT_LINE}	{}
			{STRING}		{yybegin(YYINITIAL);}
			{FALSE}			{yybegin(YYINITIAL);}
		  	{FUN_TYPE}      {location = yytext().toLowerCase(); yybegin(NAMING);}
			{END}			{if (!identifiers.isEmpty()) yybegin(END_STATE);}
			{END_FUN_TYPE}	{String funType = yytext().toLowerCase().substring(3);
							 if (!funType.contains("interface")) {
								 if(identifiers.isEmpty()){
								 	throw new JFlexException(this.getClass().getName(), parsedFileName, "Analysis failure : Identifier unreachable", yytext(), yyline, yycolumn);
								 }
								 String type = identifiers.get(identifiers.size()-1).split(" ")[0];
								 if(type.equals("block")) {
								 	if (!funType.contains("block")) {
										setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
										endComplete = true;
									}
								 }
								 else if(!type.equals(funType)) { 
									setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
									endComplete = true;
								 }
								 yybegin(END_STATE);
							 }
							 else yybegin(COMMENT);}
			{FALSE_END}		{}
			{VAR}			{yybegin(YYINITIAL);}
			{SPACE}	| {INT}	{}
			\n|\r          	{}
			.              	{yybegin(YYINITIAL);}
		}


/************************/
/* END_STATE STATE      */
/************************/
<END_STATE>		
		{
			{COMMENT_LINE}	{}

			{FUN_TYPE}		{if (!yytext().toLowerCase().contains("interface")) {
								 String type = identifiers.get(identifiers.size()-1).split(" ")[0];
								 if(type.equals("block")) {
								 	if (!yytext().toLowerCase().contains("block")) {
										setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
										endComplete = true;
									}
								 }
								 else if(!type.equals(yytext().toLowerCase())) { 
									setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
									endComplete = true;
								 }
							 }
							 else yybegin(COMMENT);
							}
			{VAR}			{if (identifiers.get(identifiers.size()-1).contains(" ")) {
								String name = "";
								if("block".equals(identifiers.get(identifiers.size()-1).split(" ")[0])) name = identifiers.get(identifiers.size()-1).split(" ")[2];
								else name = identifiers.get(identifiers.size()-1).split(" ")[1];
								if(!endComplete && !name.replaceAll("\\s+","").equals(yytext().toLowerCase().replaceAll("\\s+",""))) { 
									setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
								}
							 }
							 endComplete = true;
							}
			\n	    	    {if(!endComplete) {
								setError(identifiers.get(identifiers.size()-1),"It misses the name of the subprogram. It must finish with END TYPE_PROGRAM NAME.", yyline+1);
							 }
							 identifiers.remove(identifiers.size()-1);
							 endComplete = false;
							 yybegin(NEW_LINE);}
			.  	 	       {}
		}

/************************/
/* THROW ERROR          */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }