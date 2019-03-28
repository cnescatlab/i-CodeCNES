/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                              */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the RATEComment.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.icode.fortran90.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F90METLineOfComment
%extends AbstractChecker
%public
%ignorecase
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, DECL

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \!
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

END			 = END		  | end

%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	Float numLines = 0.0f;
	Float numComments = 0.0f;
	Float numCommentTotal = 0.0f;
	int functionLine = 0;
	boolean endLine = true;
	
	public F90METLineOfComment() {
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	//
	private void endLocation() throws JFlexException {
		final List<CheckResult> list = this.getCheckResults();      
        if (list.isEmpty()) {

    			this.computeMetric(location, numComments, functionLine + 1);
			
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(location)) {

            		last.setValue(numComments);
            	
            } else {

        			this.computeMetric(location, numComments, functionLine + 1);

			}
        }
        
        
        
        
        
	}
	
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	this.computeMetric(null, numCommentTotal, 0);
	return getCheckResults();
%eofval}

%%
				
/*********************/
/*	COMMENT PART	 */
/*********************/		
<COMMENT> 
		{
			(\n|\r)+        {numComments++; 
							 numLines++; 
							 numCommentTotal++;
							 yybegin(NEW_LINE);}  
			.              	{}
		}

/*****************/
/*	AVOID PART	 */
/*****************/		
<AVOID> 
		{
			(\n|\r)+        {numLines++;
							 yybegin(NEW_LINE);}  
			.              	{}
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		
		{
			{VAR}			{numLines = 0.0f; 
							 numComments = 0.0f; 
							 location = location + " " + yytext();
							 yybegin(DECL);}							 
			(\n|\r)+        {yybegin(NEW_LINE);}
			.              	{}
		}
		
/*****************/
/*	DECL STATE	 */
/*****************/
<DECL>		
		{
			"&"{SPACE}*[^\n\r]	{}
			"&"					{endLine = false;}
			(\n|\r)+          	{if (endLine) {
									yybegin(NEW_LINE);
								 }
								 endLine = true;
								}
			.					{}
			
		}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	
		{
			{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}        	{location = yytext(); functionLine = yyline;
							 yybegin(NAMING);}
			{SPACE}			{yybegin(NEW_LINE);}
			(\n|\r)+        {numLines = numLines + 1;
							 yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/*********************/
/*	NEW_LINE STATE	 */
/*********************/
<NEW_LINE>  	
		{
			{COMMENT_WORD} 	{yybegin(COMMENT);}
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}         	{location = yytext(); functionLine = yyline;
							 yybegin(NAMING);}
			{END}			{endLocation(); 
							 yybegin(AVOID);}
			{SPACE}			{}
			(\n|\r)+        {}
			.              	{yybegin(LINE);}
		}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); functionLine = yyline;
							 yybegin(NAMING);}
			{END}			{endLocation(); 
							 yybegin(AVOID);}
			(\n|\r)+        {numLines = numLines + 1;
							 yybegin(NEW_LINE);}
			.              	{}
		}

/*********************/
/*	ERROR THROWN	 */
/*********************/	
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}