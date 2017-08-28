/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.BLOC.File rule.		 */
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

%class F90BLOCFile
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* We had 2 states :										*/
/*    - OPEN_DEC, to deal with an open function				*/
/*    - CLOSE-DEC, the equivalent state for close method	*/
%state COMMENT, NAMING, OPEN_DEC, CLOSE_DEC

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* We had 3 words : 													*/
/*    - OPEN, which represents the open statement						*/
/*	  - CLOSE, stands for close statement								*/
/*    - UNIT, stands for unit declaration in an open or close statement	*/
OPEN		 = [^a-zA-Z0-9\_]("open"){SPACE}*"("
CLOSE		 = [^a-zA-Z0-9\_]("close"){SPACE}*"("
UNIT		 = (("unit"){SPACE}*("="))?{SPACE}*{VAR}

%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	/** Variable used to store file value and function values associated. **/
	/** List of String containing unit variable used for open statement. **/
	List<String> openUnits = new LinkedList<String>();
	/** Location corresponding to each variable stored in openUnits list. **/
	List<String> openLocation = new LinkedList<String>();
	/** Line corresponding to each variable stored in openUnits list. **/
	List<Integer> openLine = new LinkedList<Integer>();
	
	public F90BLOCFile() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	/**
	 * Method used at the end of a function / subroutine / program, to throw all errors
	 * found during analysis. This method goes through openUnits list and raise a
	 * violation for each element.
	 * @throws JFlexException 
	 **/
	private void checkOpenClose() throws JFlexException {
		for(int i = 0; i < openUnits.size(); i++) {
			setError(openLocation.get(i),"The file " + openUnits.get(i) + " is not correctly closed.", openLine.get(i));
		}
		
		// We clear every list at the end
		openUnits.clear();
		openLocation.clear();
		openLine.clear();
	}
%}

%eofval{ 
	checkOpenClose();
	return getCheckResults(); 
%eofval}


%%          
			{COMMENT_WORD}	{yybegin(COMMENT);}

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
			\n|\r           {yybegin(YYINITIAL);}
			.              	{}
		}


/****************************************************************************/
/* YYINITAL STATE       													*/
/*																			*/
/* Whenever OPEN is found, we start OPEN_DEC state. The same treatment is	*/
/* done with CLOSE word and CLOSE_DEC state.								*/
/****************************************************************************/
<YYINITIAL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			{OPEN}			{yybegin(OPEN_DEC);}
			{CLOSE}			{yybegin(CLOSE_DEC);}
			\n|\r          	{}
			.              	{}
		}

/********************************************************************/
/* OPEN_DEC STATE       											*/
/*																	*/
/* When a UNIT declaration is done, we do the following steps :		*/
/*    - if catched word contains '=', we delete spaces and take the	*/
/*		part after '=' using split									*/
/*    - if there's no '=', we only delete spaces					*/
/*    - if this "cleaned" variable is not in openUnits list, we add	*/
/*  	it (as well as location in openLocation list and line in	*/
/*		openLine list)												*/
/********************************************************************/
<OPEN_DEC>		
		{
			{UNIT}         	{String varUnit = "";
							 if (yytext().contains("=")){
								varUnit = yytext().replace(" ","").split("=")[1];
							 } else {
								varUnit = yytext().replace(" ","");
							 }
							 if(!openUnits.contains(varUnit)) {
								openUnits.add(varUnit);
							 	openLocation.add(location);
							 	openLine.add(yyline+1);
							 }
							}
			\n             	{yybegin(YYINITIAL);}
			.              	{yybegin(YYINITIAL);}
		}

/********************************************************************/
/* CLOSE_DEC STATE       											*/
/*																	*/
/* When a UNIT declaration is done, we do the following steps :		*/
/*    - if catched word contains '=', we delete spaces and take the	*/
/*		part after '=' using split									*/
/*    - if there's no '=', we only delete spaces					*/
/*    - if this "cleaned" variable is in openUnits list, we delete	*/
/*  	it (as well as location in openLocation list and line in	*/
/*		openLine list)												*/
/********************************************************************/
<CLOSE_DEC>  	
		{
			{UNIT}         	{String varUnit = "";
							 if (yytext().contains("=")){
								varUnit = yytext().replace(" ","").split("=")[1];
							 } else {
								varUnit = yytext().replace(" ","");
							 }
							 int index = openUnits.indexOf(varUnit);
							 if (index >= 0) {
							 	openUnits.remove(index);
							 	openLocation.remove(index);
							 	openLine.remove(index);
							 }
							}
			\n            	{yybegin(YYINITIAL);}
			.             	{yybegin(YYINITIAL);}
		}
	
/****************/
/* THROW ERROR	*/
/****************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }