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

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;

%%

%class COMPROJECTHeader
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, AVOID

COMMENT_WORD = \!		  | c	       | C    | \*
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
    private static final Logger LOGGER = Logger.getLogger(COMPROJECTHeader.class.getName());
	String location = "MAIN PROGRAM";
    String parsedFileName;
	
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
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
        LOGGER.finest("end method setInputFile");
	}
	
	private void addType(String type, String location, int line){
        LOGGER.finest("begin method addType");
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
        LOGGER.finest("end method addType");
	}
	
	private void raiseErrors() throws JFlexException {	
        LOGGER.finest("begin method raiseErrors");
		if(!linesType.isEmpty()){
			if (!linesType.get(0).equals("comment") && !linesType.get(1).equals("comment")){
				LOGGER.fine("Setting error line 0 because no file header (file name not found). This module/function should have a header with a brief description..");
				this.setError("No file header existing.","This module/function should have a header with a brief description.", 0);
			} else if (linesType.get(0).equals("comment") && !locations.get(0).toString().toLowerCase()
																.contains(super.getInputFile().getName().replaceFirst("[.][^.]+$", "").toLowerCase())){
				LOGGER.fine("Setting error line "+(lines.get(0))+" because no file header (file name not found). This module/function should have a header with a brief description..");
				this.setError("No file header (file name not found)."," This module/function should have a header with a brief description.", lines.get(0));
			} else if (linesType.size() > 1 && linesType.get(1).equals("comment") && !locations.get(1).toString().toLowerCase()
																.contains(super.getInputFile().getName().replaceFirst("[.][^.]+$", "").toLowerCase())){
				LOGGER.fine("Setting error line "+(lines.get(1))+" because no file header (file name not found). This module/function should have a header with a brief description..");
				this.setError("No file header (file name not found)."," This module/function should have a header with a brief description.", lines.get(1));
			}	
			
			int index = linesType.indexOf("function");
			while(index != -1){
				int prevIndex = index - 1;
				int nextIndex = index + 1;
				boolean prevIndexNoHead = prevIndex < 0 || !linesType.get(prevIndex).equals("comment")
											|| !locations.get(prevIndex).toString().toLowerCase().contains(
												locations.get(index).substring(locations.get(index).indexOf(" ")+1).toLowerCase());
				boolean nextIndexNoHead = nextIndex >= linesType.size() || !linesType.get(nextIndex).equals("comment")
											|| !locations.get(nextIndex).toString().toLowerCase().contains(
												locations.get(index).substring(locations.get(index).indexOf(" ")+1).toLowerCase());
				
				if (prevIndexNoHead && nextIndexNoHead){
					LOGGER.fine("Setting error line "+(lines.get(index))+" because the module/function should have a header with a brief description.");
					this.setError(locations.get(index).toString(),"This module/function should have a header with a brief description.", lines.get(index));
				}
				
				linesType.remove(index);
				locations.remove(index);
				lines.remove(index);
				index = linesType.indexOf("function");
			}
			LOGGER.finest("end method raiseErrors");
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
/* Violations on FUNCTION put after END.											*/
END   = end | END
SPACE = [\ \f\t]+

%%          
				{FREE_COMMENT}	{location = " ";
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
				                    yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n|\r           {this.addType("comment", location, yyline + 1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n|\\r )");
                                    yybegin(NEW_LINE);}  
<COMMENT>   	.              	{location = location + yytext();}

/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID>			\n|\r			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> NEW_LINE (Transition : \\n|\\r )");
                                    yybegin(NEW_LINE);}
<AVOID>			.				{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>    	\n|\r           {
								 if (endLine){
									this.addType("function", location, errorLine);
									first = true;
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n|\\r )");
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
<NAMING>		\&				{endLine = false;}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>     {COMMENT_WORD}	{location = " ";
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{}	
<YYINITIAL>		{FALSE}        	{}
<YYINITIAL>		{TYPE}        	{location = yytext();
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								 yybegin(NAMING);}
<YYINITIAL> 	\n|\r     		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n|\\r )");
                                    yybegin(NEW_LINE);}
<YYINITIAL>		{SPACE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : SPACE \""+yytext()+"\" )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>      {COMMENT_WORD}	{location = " ";
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}	
<NEW_LINE>		{FALSE}        	{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); 
								    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								    yybegin(NAMING);}
<NEW_LINE>		{END}			{this.addType("line", location, yyline + 1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : END \""+yytext()+"\" )");
                                    yybegin(AVOID);}
<NEW_LINE>  	\n|\r         	{}
<NEW_LINE>		{SPACE}			{}
<NEW_LINE>  	.	        	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}	
<LINE>			{FALSE}        	{}
<LINE>  		{TYPE}         	{location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<LINE>			{END}			{this.addType("line", location, yyline + 1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : END \""+yytext()+"\" )");
                                    yybegin(AVOID);}
<LINE>      	\n|\r           {this.addType("line", location, yyline + 1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n|\\r )");
                                    yybegin(NEW_LINE);}
<LINE>      	.              	{}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                               }