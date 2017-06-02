/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                              */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the NBLine.lex file, this file 			*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import java.util.logging.Logger;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F77METLineOfCode
%extends AbstractChecker
%public
%ignorecase
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \!         | c          | C     | \*
FUNC         = "FUNCTION "
PROC         = "PROCEDURE "
SUB          = "SUBROUTINE "
PROG         = "PROGRAM "
MOD          = "MODULE "
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
CLOSING		 = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END			 = END	 	  | end
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\f\t]

/* We need two variable to determine overall rate and 2 others to determine 	*/
/* comment's rate on each section (function, procedure, etc.).			*/
%{

    private static final Logger LOGGER = Logger.getLogger(F77METLineOfCode.class.getName());
    
	String location = "MAIN PROGRAM";
	float numLines = 0;
	float numTotal = 0;
	int functionLine = 0;
    String parsedFileName;
	
	
	public F77METLineOfCode(){
	}
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        this.parsedFileName = file.toString();
        LOGGER.finest("end method setInputFile");       
	}
	
	private void endLocation() throws JFlexException {
        LOGGER.finest("begin method endLocation");
		final List<CheckResult> list = this.getCheckResults(super.getInputFile());
        if (list.isEmpty()) {
        	this.computeMetric(this.location, numLines, functionLine + 1);
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numLines);
            } else {
				this.computeMetric(this.location, numLines, functionLine + 1);
			}
        }
        LOGGER.finest("end method endLocation");
	}
	
%}


/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	this.computeMetric(null, numTotal, 0);
	return getCheckResults();
%eofval}
%%

/************************************************************************/
/* Compter toutes les lignes sauf les lignes vides et les lignes en   	*/
/* commentaire.														 	*/
/************************************************************************/

				
/********************/
/* COMMNENT STATE 	*/
/********************/				
<COMMENT>
		{
			   	\n             	{
			   	                     LOGGER.finest("COMMENT -> NEW_LINE (Transition : \\n)");
                			   	     yybegin(NEW_LINE);
                			   	}  
			   	.              	{}
		}

/********************/
/* NAMING STATE 	*/
/********************/
<NAMING>
		{
				{VAR}			{
                    			     numLines = 1; 
                    			     numTotal++;
                    			     location = location + " " + yytext();
                    			     functionLine=yyline; 
                    			     LOGGER.finest("Setting values [numLines = "+numLines+" | numTotal = "+numTotal+" | location = "+location+" | functionLine = "+functionLine+"]");
                    			     LOGGER.finest("NAMING -> COMMENT (Transition : VAR)");
    							     yybegin(COMMENT);
								}							 
		    	\n             	{
                		    	     LOGGER.finest("NAMING -> NEW_LINE (Transition : \\n)");
                		    	     yybegin(NEW_LINE);
                		    	}
		    	.              	{}
		}


/********************/
/* YYINITIAL STATE 	*/
/********************/
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
				{TYPE}        	{
				                    LOGGER.finest("YYINITIAL -> NAMING (Transition : TYPE)");
				                    location = yytext();
								    yybegin(NAMING);
						        }
			 	\n             	{
			 	                   LOGGER.finest("YYINITIAL -> NEW_LINE (Transition : \\n)");
			 	                   yybegin(NEW_LINE);
			 	                }
			 	.              	{
			 	                   LOGGER.finest("YYINITIAL -> LINE (Transition : .)");
			 	                   yybegin(LINE);
			 	                }
		}


/********************/
/* NEW_LINE STATE 	*/
/********************/
<NEW_LINE>  	
		{
				{COMMENT_WORD} 	{
                				    LOGGER.finest("NEW_LINE -> COMMENT (Transition : COMMENT_WORD)");
                				    yybegin(COMMENT);
                				}
				{STRING}		{
                				    LOGGER.finest("NEW_LINE -> LINE (Transition : STRING)");
                				    yybegin(LINE);
                				}
		  		{TYPE}         	{
                    		  		location = yytext();
                    		  		LOGGER.finest("Setting value [location = "+location+"]");
                    		  		LOGGER.finest("NEW_LINE -> NAMING (Transition : TYPE)");
                    		  		yybegin(NAMING);
                		  		}
				{CLOSING}		{
                				    LOGGER.finest("NEW_LINE -> LINE (Transition : CLOSING)");
                				    yybegin(LINE);
                				}
				{END}			{
                    				numLines++;
                    				numTotal++;
                    				LOGGER.finest("Setting values [numLines = "+numLines+" | numTotal = "+numTotal+"]");
                    				endLocation();
                    				LOGGER.finest("NEW_LINE -> COMMENT (Transition : END)");
                    				yybegin(COMMENT);
                				}
				{VAR}			{
                				    LOGGER.finest("NEW_LINE -> LINE (Transition : VAR)");
                				    yybegin(LINE);
                				}
				{SPACE}			{}
			 	\n             	{}
			  	.              	{
                                    LOGGER.finest("NEW_LINE -> LINE (Transition : .)");
                                    yybegin(LINE);
                			  	}
		}


/********************/
/* LINE STATE 		*/
/********************/
<LINE>	
		{
				{STRING}		{}
		  		{TYPE}         	{
                                    location = yytext();
                                    LOGGER.finest("Setting value [location = "+location+"]");
    								LOGGER.finest("LINE -> NAMING (Transition : TYPE)");
    								yybegin(NAMING);
								}
				{CLOSING}		{}
				{END}			{
                    				numLines++;
                    				numTotal++;
                    				LOGGER.finest("Setting values [numLines = "+numLines+" | numTotal = "+numTotal+"]");
                                    endLocation();
                    				LOGGER.finest("LINE -> COMMENT (Transition : END)");
                    				yybegin(COMMENT);
				                }
				{VAR}			{}
		      	\n             	{
                    		      	numLines++;
                    		      	numTotal++;
                    		      	LOGGER.finest("Setting values [numLines = "+numLines+" | numTotal = "+numTotal+"]");
                    		      	LOGGER.finest("LINE -> NEW_LINE (Transition : \\n)");
                    				yybegin(NEW_LINE);
                				}
		      	.              	{}
		}


/********************/
/* DEFAULT STATE 	*/
/********************/
				[^]            {
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+yyline+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                               }
