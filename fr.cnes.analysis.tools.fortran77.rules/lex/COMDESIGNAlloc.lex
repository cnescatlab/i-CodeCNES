/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Alloc rule.	 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDESIGNAlloc
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, OPEN, CLOSE, ALLOC, DEALLOC

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

ALLOCATE	 = [^a-zA-Z0-9\_]("allocate")[^a-zA-Z0-9\_]
DEALLOCATE	 = [^a-zA-Z0-9\_]("deallocate")[^a-zA-Z0-9\_]
OPEN		 = [^a-zA-Z0-9\_]("open")[^a-zA-Z0-9\_]
CLOSE		 = [^a-zA-Z0-9\_]("close")[^a-zA-Z0-9\_]
UNIT		 = ("unit")
INT			 = [0-9]+
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMDESIGNAlloc.class.getName());

	String location = "MAIN PROGRAM";
	Map<String, String> memory = new HashMap<String, String>();
	Map<String, String> files  = new HashMap<String, String>();
	Map<String, Integer> lines = new HashMap<String, Integer>();
	List<String> errors = new LinkedList<String>();
    String parsedFileName;
	
	public COMDESIGNAlloc(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	/**
     * Method used to throw errors due to allocate or open, not followed by a
     * deallocate or a close.
     **/
    public void raiseRemainingErrors() throws JFlexException {
        LOGGER.finest("begin method raiseRemainingErrors");
        Iterator<Entry<String, String>> iterator =
                this.memory.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            LOGGER.fine("Setting error line "+this.lines.get(pairs.getKey())+" for the variable "+pairs.getKey()+".");
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }

        iterator = this.files.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            LOGGER.fine("Setting error line "+this.lines.get(pairs.getKey())+" for the variable "+pairs.getKey()+".");
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }
        LOGGER.finest("end method raiseRemainingErrors");
    }
	
	/**
     * Sort all violations.
     **/
    public void sortResults() {
        LOGGER.finest("begin method sortResults");
        Collections.sort(getCheckResults(), new Comparator<CheckResult>() {
            @Override
            public int compare(final CheckResult result1, final CheckResult result2) {
                int res = result1.getId().compareTo(result2.getId());
                if (res == 0) {
                    res =
                            result1.getFile()
                                    .getName()
                                    .compareTo(
                                            result2.getFile()
                                                    .getName());
                    if (res == 0) {
                        res = result1.getLine().compareTo(result2.getLine());
                        if (res == 0) {
                            res =
                                    result1.getLocation().compareTo(
                                            result2.getLocation());
                        }
                    }
                }
                return res;
            }
        });
        LOGGER.finest("end method sortResults");
    }
		
%}

%eofval{
	raiseRemainingErrors();
	sortResults();
	return getCheckResults();
%eofval}


%%          

/************************/

			{FREE_COMMENT}	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
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
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                    			location = location + " " + yytext();
                    			yybegin(COMMENT);
                			}
			\n             	{
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
			{COMMENT_WORD} 	{
                      			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                      			yybegin(COMMENT);
                			}
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
			{COMMENT_WORD} 	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                    			yybegin(COMMENT);
                			}
			{STRING}		{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                    			yybegin(LINE);
                			}
			{FALSE}			{}
			{TYPE}         	{
                    			location = yytext();
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                    			yybegin(NAMING);
                			}
			{OPEN}			{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> OPEN (Transition : OPEN \""+yytext()+"\" )");
                    			yybegin(OPEN);
                			}
			{ALLOCATE}		{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> ALLOC (Transition : ALLOCATE \""+yytext()+"\" )");
                    			yybegin(ALLOC);
                			}
			{CLOSE}			{
                		      	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> CLOSE (Transition : CLOSE \""+yytext()+"\" )");
                		      	yybegin(CLOSE);
                			}
			{DEALLOCATE}	{
                		      	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DEALLOC (Transition : DEALLOCATE \""+yytext()+"\" )");
                		      	yybegin(DEALLOC);
                			}
			\n             	{}
			.              	{
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
                       			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                       			yybegin(COMMENT);
                			}
			{STRING}		{
                     			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                     			yybegin(LINE);
                			}
			{TYPE}         	{
                    			location = yytext();
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                    			yybegin(NAMING);
                			}
			{OPEN}			{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> OPEN (Transition : OPEN \""+yytext()+"\" )");
                    			yybegin(OPEN);
                			}
			{ALLOCATE}		{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> ALLOC (Transition : ALLOCATE \""+yytext()+"\" )");
                    			yybegin(ALLOC);
                			}
			{CLOSE}			{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> CLOSE (Transition : CLOSE \""+yytext()+"\" )");
                    			yybegin(CLOSE);
                			}
			{DEALLOCATE}	{
                       			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DEALLOC (Transition : DEALLOCATE \""+yytext()+"\" )");
                       			yybegin(DEALLOC);
                			}
			\n             	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                    			yybegin(NEW_LINE);
                			}
			.              	{}
		}

/************************/
/* OPEN STATE    	    */
/************************/
<OPEN>			
		{
			{UNIT}			{}
			{VAR}|{INT}		{
                    			files.put(yytext(), location);
                    			lines.put(yytext(), yyline+1);
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - OPEN -> COMMENT (Transition : {VAR}|{INT} \""+yytext()+"\" )");
                    			yybegin(COMMENT);
                			}
			\n             	{}
			.              	{}
		}

/************************/
/* CLOSE STATE    	    */
/************************/
<CLOSE>			
		{
			{UNIT}			{}
			{VAR}|{INT}		{
        		                 String loc = files.get(yytext());
        						 if (!location.equals(loc) && !errors.contains(yytext())) {
        							LOGGER.fine("Setting error line "+(yyline+1)+" for the variable "+ yytext()+".");
        							setError(location,"The resource named "+
                							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
        						 }
        						 if (loc != null) {
        							errors.add(yytext());
        						 }
        						 files.remove(yytext()); 
        						 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - CLOSE -> COMMENT (Transition : {VAR}|{INT} \""+yytext()+"\" )");
        						 yybegin(COMMENT);
							}
			\n             	{}
			.              	{}
		}

/************************/
/* ALLOC STATE    	    */
/************************/
<ALLOC>			
		{
			{VAR}			{
    			                 memory.put(yytext(), location); 
    							 lines.put(yytext(), yyline+1);
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ALLOC -> COMMENT (Transition : VAR \""+yytext()+"\" )");
    							 yybegin(COMMENT);}
			\n             	{}
			.              	{}
		}

/************************/
/* DEALLOC STATE    	    */
/************************/
<DEALLOC>		
		{
			{VAR}			{
    			                 String loc = memory.get(yytext());
    							 if (!location.equals(loc) && !errors.contains(yytext())) {
    								LOGGER.fine("Setting error line "+(yyline+1)+" for the variable "+ yytext()+".");
    								setError(location,"The resource named "+
                							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
    							 }
    							 if (loc != null) {
    								errors.add(yytext());
    							 }
    							 memory.remove(yytext()); 
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DEALLOC -> COMMENT (Transition : VAR \""+yytext()+"\" )");
    							 yybegin(COMMENT);
							}
			\n             	{}
			.              	{}
		}
			
/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }