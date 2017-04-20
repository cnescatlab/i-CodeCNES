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
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

%%

%class F77METLineOfCode
%extends AbstractMetric
%public
%ignorecase

%line
%column

%function run
%yylexthrow JFlexException
%type FileValue

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
	FileValue fileValue;
	float numLines = 0;
	float numTotal = 0;
	int functionLine = 0;
    String parsedFileName;
	
	
	public F77METLineOfCode(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
        LOGGER.finest("begin method setInputFile");
		fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
        this.parsedFileName = file.toString();
		this.zzReader = new FileReader(file.toOSString());
        LOGGER.finest("end method setInputFile");       
	}
	
	private void endLocation() {
        LOGGER.finest("begin method endLocation");
		final List<FunctionValue> list =
                this.fileValue.getFunctionValues();
        if (list.isEmpty()) {
            list.add(new FunctionValue(this.location, numLines, functionLine+1));
        } else {
            final FunctionValue last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numLines);
            } else {
				list.add(new FunctionValue(this.location, numLines, functionLine+1));
			}
        }
		
        LOGGER.finest("end method endLocation");
	}
	
%}


/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
	fileValue.setValue(numTotal);
	return fileValue;
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
