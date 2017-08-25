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

package fr.cnes.analysis.tools.fortran90.rules;

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

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDESIGNAlloc
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, OPEN, CLOSE, ALLOC, DEALLOC

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
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	Map<String, String> memory = new HashMap<String, String>();
	Map<String, String> files  = new HashMap<String, String>();
	Map<String, Integer> lines = new HashMap<String, Integer>();
	List<String> errors = new LinkedList<String>();
	
	public COMDESIGNAlloc(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/**
     * Method used to throw errors due to allocate or open, not followed by a
     * deallocate or a close.
     **/
    public void raiseRemainingErrors() throws JFlexException {
        Iterator<Entry<String, String>> iterator =
                this.memory.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }

        iterator = this.files.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }
    }
	
	/**
     * Sort all violations.
     **/
    public void sortResults() {
        Collections.sort(getCheckResults(), new Comparator<CheckResult>() {
            @Override
            public int compare(final CheckResult viol1, final CheckResult viol2) {
                int res = viol1.getId().compareTo(viol2.getId());
                if (res == 0) {
                    res =
                            viol1.getFile()
                                    .getName()
                                    .compareTo(
                                            viol2.getFile()
                                                    .getName());
                    if (res == 0) {
                        res = viol1.getLine().compareTo(viol2.getLine());
                        if (res == 0) {
                            res =
                                    viol1.getLocation().compareTo(
                                            viol2.getLocation());
                        }
                    }
                }
                return res;
            }
        });
    }
		
%}

%eofval{
	raiseRemainingErrors();
	sortResults();
	return getCheckResults();
%eofval}


%%          

/************************/

			{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE	        */
/************************/
<NAMING>
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	
		{
			{TYPE}        	{location = yytext(); 
							 yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{STRING}		{yybegin(LINE);}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{OPEN}			{yybegin(OPEN);}
			{ALLOCATE}		{yybegin(ALLOC);}
			{CLOSE}			{yybegin(CLOSE);}
			{DEALLOCATE}	{yybegin(DEALLOC);}
			\n             	{}
			.              	{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{STRING}		{yybegin(LINE);}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{OPEN}			{yybegin(OPEN);}
			{ALLOCATE}		{yybegin(ALLOC);}
			{CLOSE}			{yybegin(CLOSE);}
			{DEALLOCATE}	{yybegin(DEALLOC);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* OPEN STATE    	    */
/************************/
<OPEN>			
		{
			{UNIT}			{}
			{VAR}|{INT}		{files.put(yytext(), location); 
							 lines.put(yytext(), yyline+1);
							 yybegin(COMMENT);}
			\n             	{}
			.              	{}
		}

/************************/
/* CLOSE STATE    	    */
/************************/
<CLOSE>			
		{
			{UNIT}			{}
			{VAR}|{INT}		{String loc = files.get(yytext());
							 if (!location.equals(loc) && !errors.contains(yytext())) {
								setError(location,"The resource named "+
            							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
							 }
							 if (loc != null) {
								errors.add(yytext());
							 }
							 files.remove(yytext()); 
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
			{VAR}			{memory.put(yytext(), location); 
							 lines.put(yytext(), yyline+1);
							 yybegin(COMMENT);}
			\n             	{}
			.              	{}
		}

/************************/
/* DEALLOC STATE    	    */
/************************/
<DEALLOC>		
		{
			{VAR}			{String loc = memory.get(yytext());
							 if (!location.equals(loc) && !errors.contains(yytext())) {
								setError(location,"The resource named "+
            							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
							 }
							 if (loc != null) {
								errors.add(yytext());
							 }
							 memory.remove(yytext()); 
							 yybegin(COMMENT);
							}
			\n             	{}
			.              	{}
		}
			
/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }