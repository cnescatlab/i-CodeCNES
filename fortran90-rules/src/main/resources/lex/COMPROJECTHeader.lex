/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Pr.CartStd rule.		 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;

%%

%class COMPROJECTHeader
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, AVOID

COMMENT_WORD = \!
FREE_COMMENT = \!
FUNC         = "function"
PROC         = "procedure"
SUB          = "subroutine"
PROG         = "program"
MOD          = "module"
INTERFACE	 = "interface"
TYPE		 = {FUNC} | {PROC} | {SUB} | {PROG} | {MOD} | {INTERFACE}
MOD_PROC     = {MOD}{SPACE}*{PROC}
FALSE        = [a-zA-Z0-9\_]({FUNC} | {PROC} | {SUB} | {PROG} | {MOD} | {INTERFACE})
			   | ({FUNC} | {PROC} | {SUB} | {PROG} | {MOD} | {INTERFACE})[a-zA-Z0-9\_]
			   | [a-zA-Z0-9\_]({FUNC} | {PROC} | {SUB} | {PROG} | {MOD} | {INTERFACE})[a-zA-Z0-9\_]
			   | {MOD_PROC}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* A boolean called comment determines if a comment is set. */
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> linesType = new LinkedList<String>();
	List<StringBuilder> locations = new LinkedList<StringBuilder>();
	List<Integer> lines = new LinkedList<Integer>();
	boolean endLine = true;
	boolean first = true;
	int errorLine = 0;
	
	
	public COMPROJECTHeader() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	
	
	
	private void addType(String type, String location, int line){
		if (linesType.isEmpty()){
			linesType.add(type);
			StringBuilder buffer = new StringBuilder();
			buffer.append(location);
			locations.add(buffer);
			lines.add(line);
		} else {
			int last = linesType.size()-1;
			if (linesType.get(last).equals("comment") && type.equals("comment")){
				locations.get(last).append(location);
			} else {
				linesType.add(type);
				StringBuilder buffer = new StringBuilder();
				buffer.append(location);
				locations.add(buffer);
				lines.add(line);
			}
		}
	}
	
	private void raiseErrors() throws JFlexException {
		if(!this.linesType.isEmpty()){
			if (!this.linesType.get(0).equals("comment")
					&& !this.linesType.get(1).equals("comment")) {
				this.setError("No file header existing.","This module/function should have a header with a brief description.", 0);
			} else if (this.linesType.get(0).equals("comment")
					&& !this.locations
							.get(0)
							.toString()
							.toLowerCase()
							.contains(
									super.getInputFile()
											.getName()
											.replaceFirst("[.][^.]+$", "")
											.toLowerCase())) {
				this.setError("No file header (file name not found)","This module/function should have a header with a brief description.",
						this.lines.get(0));
			} else if (linesType.size() > 1 && this.linesType.get(1).equals("comment")
					&& !this.locations
							.get(1)
							.toString()
							.toLowerCase()
							.contains(
									super.getInputFile()
											.getName()
											.replaceFirst("[.][^.]+$", "")
											.toLowerCase())) {
				this.setError("No file header (file name not found)"," This module/function should have a header with a brief description.",
						this.lines.get(1));
			}

			int index = this.linesType.indexOf("function");
			while (index != -1) {
				final int prevIndex = index - 1;
				final int nextIndex = index + 1;
				final boolean prevIndexNoHead =
						prevIndex < 0
								|| !this.linesType.get(prevIndex)
										.equals("comment")
								|| !this.locations
										.get(prevIndex)
										.toString()
										.toLowerCase()
										.contains(
												this.locations
														.get(index)
														.substring(
																this.locations
																		.get(index)
																		.indexOf(
																				" ") + 1)
														.toLowerCase());
				final boolean nextIndexNoHead =
						nextIndex >= this.linesType.size()
								|| !this.linesType.get(nextIndex)
										.equals("comment")
								|| !this.locations
										.get(nextIndex)
										.toString()
										.toLowerCase()
										.contains(
												this.locations
														.get(index)
														.substring(
																this.locations
																		.get(index)
																		.indexOf(
																				" ") + 1)
														.toLowerCase());

				if (prevIndexNoHead && nextIndexNoHead) {
					this.setError(this.locations.get(index).toString(),"This module/function should have a header with a brief description.",
							this.lines.get(index));
				}

				this.linesType.remove(index);
				this.locations.remove(index);
				this.lines.remove(index);
				index = this.linesType.indexOf("function");
			}
		}
    }
%}

%eofval{
	raiseErrors();
	linesType.clear();
	locations.clear();
	lines.clear();
    
	
	return getCheckResults();
%eofval}
%eofclose

/* There is no specific rule word in this rule. However, we define END to avoid */
/* CheckResult on FUNCTION put after END.											*/
END   = end | END
SPACE = [\ \f\t]+

%%          
				{FREE_COMMENT}	{location = " ";yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n|\r           {this.addType("comment", location, yyline + 1);yybegin(NEW_LINE);}  
<COMMENT>   	.              	{location = location + yytext();}

/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID>			\n|\r			{yybegin(NEW_LINE);}
<AVOID>			.				{}


/************************/
/* NAMING STATE	        */
/************************/

<NAMING>    	\n|\r           {
								 if (endLine){
									this.addType("function", location, errorLine);
									first = true;
									yybegin(NEW_LINE);
								 }
								 endLine = true;
								}
<NAMING>		{VAR}			{if (first){
									errorLine = yyline + 1;
									location = location + " " + yytext();
									first = false;
								 }
								}
<NAMING>		"&"				{endLine = false;}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>     {COMMENT_WORD}	{location = " ";yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{}	
<YYINITIAL>		{FALSE}        	{}
<YYINITIAL>		{TYPE}        	{location = yytext();
								 yybegin(NAMING);}
<YYINITIAL> 	\n|\r     		{yybegin(NEW_LINE);}
<YYINITIAL>		{SPACE}			{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>      {COMMENT_WORD}	{location = " ";yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}	
<NEW_LINE>		{FALSE}        	{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); 
								 yybegin(NAMING);}
<NEW_LINE>		{END}			{this.addType("line", location, yyline + 1);yybegin(AVOID);}
<NEW_LINE>  	\n|\r         	{}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	.	        	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}	
<LINE>			{FALSE}        	{}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{END}			{this.addType("line", location, yyline + 1);yybegin(AVOID);}
<LINE>      	\n|\r           {this.addType("line", location, yyline + 1);yybegin(NEW_LINE);}
<LINE>      	.              	{}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }