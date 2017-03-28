/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DESIGN.ActiveWait rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;
import java.util.LinkedList;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDESIGNActiveWait
%extends AbstractRule
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

END			 = "end"{SPACE}*"do"
WAIT		 = "sleep" 	| "wait"	| "pause"

%{
    private static final Logger LOGGER = Logger.getLogger(COMDESIGNActiveWait.class.getName());
	
	String location = "MAIN PROGRAM";
	List<String> errors = new LinkedList<String>();
	int loop = 0;
    String parsedFileName;
	
	
	public COMDESIGNActiveWait(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
		this.zzReader = new FileReader(file.toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
		
%}

%eofval{
	return getViolations();
%eofval}


%%          

/************************/

			{FREE_COMMENT}	{
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT )");
                                yybegin(COMMENT);
                            }

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n             	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                    			yybegin(NEW_LINE);
                			}  
			.              	{}
		}

/************************/
/* NAMING STATE	        */
/************************/
<NAMING>
		{
			{VAR}			{
                    			location = location + " " + yytext();
                    			loop=0;
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                    			yybegin(COMMENT);
                			}
			\n             	{
			                    loop=0;
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                yybegin(NEW_LINE);
                            }
			.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	
		{
			{TYPE}        	{
                    			location = yytext();
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                yybegin(NAMING);
                            }
			\n             	{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                			     yybegin(NEW_LINE);
                			}
			.              	{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                			     yybegin(LINE);
                			}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{COMMENT_WORD} 		{
                    			    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                    			    yybegin(COMMENT);
                    			}
			{STRING}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);
                    			}
			{FALSE}				{}
			{TYPE}         		{
                        			location = yytext();
                        			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
			"do"				{loop++;}
			{END}				{loop--;}
			"continue"			{loop=0;}
			{WAIT}				{
                        			if(loop>0){
                        			     LOGGER.fine("Setting error line "+yyline+1+" at the location "+location+".");
                        			     this.setError(location,"This process contains an active wait.", yyline+1);
                        		    }
                    		    }
			{VAR}				{}
			\n             		{}
			.              		{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                    			     yybegin(LINE);
                    			}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{COMMENT_WORD} 	{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT_WORD -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                			     yybegin(COMMENT);
                			}
			{STRING}		{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT_WORD -> LINE (Transition : STRING \""+yytext()+"\" )");
                    			yybegin(LINE);
                			}
			{TYPE}         	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT_WORD -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                    			location = yytext();
                    			yybegin(NAMING);
                			}
			"do"				{loop++;}
			{END}			{loop--;}
			"continue"		{loop=0;}
			{WAIT}			{
                    			if(loop>0){
                    			     LOGGER.fine("Setting error line "+yyline+1+" at the location "+location+".");
                    			     this.setError(location,"This process contains an active wait.", yyline+1);
                			    }
                		    }
			{VAR}			{}
			\n             	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT_WORD -> NEW_LINE (Transition : \\n )");
                    			yybegin(NEW_LINE);
                			}
			.              	{}
		}
			
/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+(yyline+1)+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                                }