/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                             */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the RATEComment.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;
import java.io.File;
import java.util.logging.Logger;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
%%

%class F77METRatioComment
%extends AbstractChecker
%public
%ignorecase
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, DECL

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \!	| "c"	|\*	
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

END			 = END		  | end

%{
	private static final Logger LOGGER = Logger.getLogger(F77METRatioComment.class.getName());
	
	
	String location = "MAIN PROGRAM";
	Float numLines = 0.0f;
	Float numComments = 0.0f;
	boolean endLine = true;
	int functionLine = 0;
    String parsedFileName;
	
	
	public F77METRatioComment() {
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        this.parsedFileName = file.toString();
        LOGGER.finest("end method setInputFile");       
	}
	
	private void endLocation() throws JFlexException{
        LOGGER.finest("begin method endLocation");
		final List<CheckResult> list = this.getCheckResults();
        if (list.isEmpty()) {
        	if (numLines<10){
        		this.computeMetric(location, Float.NaN, functionLine+1);
        	} else { 
        		this.computeMetric(location, (numComments/(numLines))*100, functionLine+1);
        	}
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(location)) {
                if (numLines<10) {
                	last.setValue(Float.NaN);
        		} else { 
        			last.setValue((numComments/(numLines))*100);
            	}
            } else {
				if (numLines<10){
        			this.computeMetric(location, Float.NaN, functionLine+1);
	        	} else { 
	        		this.computeMetric(location, (numComments/(numLines))*100, functionLine+1);
	        	}
			}
        }
        LOGGER.finest("end method endLocation");
	}
	
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	this.computeMetric(null, Float.NaN, 0);
	return getCheckResults();
%eofval}

%%
				
/*********************/
/*	COMMENT PART	 */
/*********************/		
<COMMENT> 
		{
			(\n|\r)+        {
                                numComments++; 
                                numLines++; 
                                LOGGER.finest("Setting values [numComments ="+numComments+" | numLines = "+ numLines+"]");
                                LOGGER.finest("COMMENT -> NEW_LINE (Transition : \\n | \\r)");
                                yybegin(NEW_LINE);
                            }  
			.              	{}
		}

/*****************/
/*	AVOID PART	 */
/*****************/		
<AVOID> 
		{
			(\n|\r)+        {
			                     numLines++;
			                     LOGGER.finest("Setting value [numLines ="+numLines+"]");
			                     LOGGER.finest("AVOID -> NEW_LINE (Transition : \\n | \\r)");
							     yybegin(NEW_LINE);
							}  
			.              	{}
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		
		{
			{VAR}			{
    			                 numLines = 0.0f; 
    							 numComments = 0.0f; 
    							 functionLine = yyline;
    							 location = location + " " + yytext();
                                 LOGGER.finest("Setting values [numComments ="+numComments+" | numLines = "+ numLines+" | functionLine = "+ functionLine+" | location = "+ location+"]");
    							 LOGGER.finest("NAMING -> DECL (Transition : VAR)");
    							 yybegin(DECL);
							 }							 
			(\n|\r)+        {
                			     LOGGER.finest("NAMING -> NEW_LINE (Transition : \\n | \\r)");
                			     yybegin(NEW_LINE);
                			}
			.              	{}
		}
		
/*****************/
/*	DECL STATE	 */
/*****************/
<DECL>		
		{
			"&"{SPACE}*[^\n\r]	{}
			"&"					{
			                        endLine = false;
			                        LOGGER.finest("Setting value [endLine ="+endLine+"]");
			                    }
			(\n|\r)+          	{
                                    if (endLine) {
                                        LOGGER.finest("DECL -> NEW_LINE (Transition : \\n | \\r && endLine = true)");
                                        yybegin(NEW_LINE);
                                    }
                                    endLine = true;
                                    LOGGER.finest("Setting value [endLine ="+endLine+"]");
								}
			.					{}
			
		}

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	
		{
			{COMMENT_WORD} 	{
			                     LOGGER.finest("YYINITIAL -> COMMENT (Transition : COMMENT_WORD)");
			                     yybegin(COMMENT);
			                }
			{STRING}		{  
			                     LOGGER.finest("YYINITIAL -> LINE (Transition : STRING)");
			                     yybegin(LINE);
		                    }
			{FALSE}			{
                			     LOGGER.finest("YYINITIAL -> LINE (Transition : FALSE)");
                			     yybegin(LINE);
                			}
			{TYPE}        	{
                    			location = yytext(); 
                    			LOGGER.finest("Setting value [location ="+location+"]");
                    			LOGGER.finest("YYINITIAL -> NAMING (Transition : TYPE)");
                    			yybegin(NAMING);
                			}
			{SPACE}			{
			                    LOGGER.finest("YYINITIAL -> NEW_LINE (Transition : SPACE)");
			                    yybegin(NEW_LINE);
			                }
			(\n|\r)+        {
    			                 numLines = numLines + 1;
    			                 LOGGER.finest("Setting value [numLines ="+numLines+"]");
    							 LOGGER.finest("YYINITIAL -> NEW_LINE (Transition : \\n | \\r)");
    							 yybegin(NEW_LINE);
    					    }
			.              	{
                			     LOGGER.finest("YYINITIAL -> NEW_LINE (Transition : .)");
                			     yybegin(LINE);
                			}
		}

/*********************/
/*	NEW_LINE STATE	 */
/*********************/
<NEW_LINE>  	
		{
			{COMMENT_WORD} 	{
    			                 if (yycolumn == 0) {
    			                    LOGGER.finest("NEW_LINE -> COMMENT (Transition : COMMENT_WORD && yycolumn == 0)");
    								yybegin(COMMENT);
    							 } else {
    							    LOGGER.finest("NEW_LINE -> LINE (Transition : COMMENT_WORD && yycolumn != 0)");
    								yybegin(LINE);
    							 }
							}
			{STRING}		{
                			     LOGGER.finest("NEW_LINE -> LINE (Transition : STRING");
                			     yybegin(LINE);
                			}
			{FALSE}			{
			                     LOGGER.finest("NEW_LINE -> LINE (Transition : FALSE");
			                     yybegin(LINE);
			                }    
			{TYPE}         	{
    			                 location = yytext(); 
    			                 LOGGER.finest("Setting value [location ="+location+"]");
    							 LOGGER.finest("NEW_LINE -> NAMING (Transition : TYPE");
    							 yybegin(NAMING);
							}
			{END}			{
    			                 endLocation(); 
    			                 LOGGER.finest("NEW_LINE -> AVOID (Transition : END");
    							 yybegin(AVOID);
							 }
			{SPACE}			{}
			(\n|\r)+        {}
			.              	{
                			     LOGGER.finest("NEW_LINE -> LINE (Transition : .");
                			     yybegin(LINE);
                			}
		}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{
			                     location = yytext();
			                     LOGGER.finest("Setting value [location ="+location+"]");
			                     LOGGER.finest("LINE -> NAMING (Transition : TYPE");
							     yybegin(NAMING);
							}
			{END}			{
    			                 endLocation(); 
    							 LOGGER.finest("LINE -> AVOID (Transition : END");
    							 yybegin(AVOID);
							 }
			(\n|\r)+        {
    			                 numLines = numLines + 1;
    			                 LOGGER.finest("Setting value [numLines ="+numLines+"]");
    			                 LOGGER.finest("LINE -> NEW_LINE (Transition : \\n | \\r)");
    							 yybegin(NEW_LINE);
							}
			.              	{}
		}

/*********************/
/*	ERROR THROWN	 */
/*********************/	
			[^]      	{
            			        String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+yyline+" column:"+yycolumn;
                                throw new JFlexException(new Exception(errorMessage));
            			}