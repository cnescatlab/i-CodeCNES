/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.ERR.OpenRead rule	 */
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
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class F90ERROpenRead
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, OPEN, READ, IF_STATE

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

LINE_COMMENT = ("!"|"#")[^\n]*\n
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

OPEN		 = "open"{SPACE}*\(
READ		 = "read"{SPACE}*\(
IF_IOS		 = "if" {SPACE}* \( {SPACE}* {VAR}
IOSTAT 		 = ("iostat") {SPACE}* \= {SPACE}* {VAR}

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
%{
	String location = "MAIN PROGRAM"; 
    List<Violation> list = new LinkedList<Violation>();
	boolean iostat = false, file = false, add=false;
	boolean multLines = false;
	List<String> files = new LinkedList<String>();
	int errorLine = 0;
	String descr = "", iostatVal = "";
	
	public F90ERROpenRead() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
	return getViolations();
%eofval}


%%          

/********************/
/* COMMENT STATE	*/
/********************/
<COMMENT>   	
		{
			\n|\r           {yybegin(YYINITIAL);}  
			.              	{}
		}


/****************/
/* NAMING STATE	*/
/****************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n|\r			{yybegin(YYINITIAL);}
			.              	{}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		
		{
			{LINE_COMMENT}	{}
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{OPEN}			{errorLine = yyline + 1; descr = yytext().toUpperCase().replaceAll("\\(", "").trim(); iostat=false; file=false; add=false; yybegin(OPEN);}
			{READ}			{errorLine = yyline + 1; descr = yytext().toUpperCase().replaceAll("\\(", "").trim(); iostat=false; add=false; yybegin(READ);}
			{VAR}			{}
			\n|\r			{}
			.              	{}
		}

/************************/
/* OPEN STATE     		*/
/************************/
<OPEN>		
		{
			{LINE_COMMENT}   	{}
			{STRING} | "unit"	{}
			"file"				{file=true;}
			{IOSTAT}			{iostat = true;
								 iostatVal = yytext().replace(" ","").split("=")[1];}
			{VAR} | \*			{if(!add) {files.add(yytext()); add=true;} }
			&{SPACE}*[^\n\r]	{}
			&					{multLines = true;}
			\n					{if(!multLines) {
									if (!iostat&&file) {
										this.setError(location,"There is no parameter IOSTAT in the " + descr + " instruction.", errorLine);
										yybegin(YYINITIAL);
									}
									else if(iostat&&file) {
										yybegin(IF_STATE);
									}
									else {
										files.remove(files.size()-1); //delete the last element -> this instruction does not open a file
										yybegin(YYINITIAL);
									}
								 } 
								 multLines = false;
								}
			.					{}
		}


/************************/
/* READ STATE      		*/
/************************/
<READ>		
		{
			{LINE_COMMENT}   	{}
			{STRING} | "unit"	{}
			{IOSTAT}			{iostat = true;
								 iostatVal = yytext().replace(" ","").split("=")[1];}
			{VAR}				{if(!add) {     // if the first value read and existing file opened -> verify iostat
								 	if (!files.contains(yytext())) {
								 		yybegin(YYINITIAL);
								 	} else {
								 		add=true;
								 }}}
			&{SPACE}*[^\n\r]	{}
			&					{multLines = true;}
			\n					{if(!multLines) {
									if (!iostat) {
										this.setError(location,"There is no parameter IOSTAT in the " + descr + " instruction.", errorLine);
										yybegin(YYINITIAL);
									}
									else
										yybegin(IF_STATE);
								 } 
								 multLines = false;
								}
			.					{}
		}

/************************/
/* IF_STATE STATE       */
/************************/
<IF_STATE>		
		{
			{LINE_COMMENT}		{}
			{IF_IOS}			{String checkedVal = yytext().replace(" ","").split("\\(")[1];
								 if (!checkedVal.equals(iostatVal)) this.setError(location,"The return of IOSTAT is no checked in the " + descr + " instruction.", errorLine); 
								 yybegin(YYINITIAL);}
			{OPEN}				{this.setError(location,"The return of IOSTAT is no checked in the " + descr + " instruction.", errorLine);
								 errorLine = yyline + 1; descr = yytext().toUpperCase().replaceAll("\\(", "").trim(); iostat=false; file=false; add=false; yybegin(OPEN);}
			{READ}				{this.setError(location,"The return of IOSTAT is no checked in the " + descr + " instruction.", errorLine);
								 errorLine = yyline + 1; descr = yytext().toUpperCase().replaceAll("\\(", "").trim(); iostat=false; add=false; yybegin(READ);}
			{VAR}				{this.setError(location,"The return of IOSTAT is no checked in the " + descr + " instruction.", errorLine);
								 yybegin(YYINITIAL);}
			&{SPACE}*[^\n\r]	{}
			\n|\r				{}
			.					{}
		}		
		
/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
