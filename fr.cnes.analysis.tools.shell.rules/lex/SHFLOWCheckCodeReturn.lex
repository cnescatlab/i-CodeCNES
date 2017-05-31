/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.FLOW.CheckCodeReturn rule. */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class SHFLOWCheckCodeReturn
%extends AbstractChecker
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CHECKRET, FINLINE, COMMENTARGS, PIPELINE

COMMENT_WORD = \#
SPACE		 = [\ \r\t\f]
FUNCTION	 = "function"
FUNC		 = {VAR}{SPACE}*\(\)
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"


																
%{
	private String location = "MAIN PROGRAM";
	/** Map with each function name and if contains a return or not **/
	private Map<String,Boolean> functions = new HashMap<String,Boolean>();
	/** The return of the last function is checked: avoid problems with last call **/
	private boolean verified = false;
	private boolean functionCalled = false;
	private boolean pipeline = false;
	/** Number of lines since last function call **/
	private int linesError = 1;
	/** Last function called **/
	private String functionCall = "";

    public SHFLOWCheckCodeReturn() {
    	/** The command 'cd' must be checked as the functions **/
		functions.put("awk",true);
		functions.put("batch",true);
		functions.put("cat",true);
    	functions.put("cd",true);
		functions.put("chgrp",true);
		functions.put("chmod",true);
		functions.put("chown",true);
		functions.put("command",true);
		functions.put("compress",true);
		functions.put("cp",true);
		functions.put("expr",true);
		functions.put("find",true);
		functions.put("kill",true);
		functions.put("ls",true);
		functions.put("mkdir",true);
		functions.put("mv",true);
		functions.put("read",true);
		functions.put("rm",true);
		functions.put("rmdel",true);
		functions.put("rmdir",true);
		functions.put("sed",true);
		functions.put("touch",true);
		functions.put("uncompress",true);
		functions.put("write",true);
		functions.put("xargs",true);
    }
	
    /** 
      * addViolation: adds a violation on the function named functionCall
      * The violation is added either when there is a new function call, or 
	  * when the EOF is reached, so is stored, along with the number of lines ago(linesError).
	  * If the violation is on a pipeline, functionCall contains all the functions of the  
	  * pipeline that are in the functions list.
      */
    private void addViolation() throws JFlexException {
        if(pipeline == true) {
            setError(location, "The return status of pipeline with function(s) " + functionCall + " has not been checked.", yyline+1-linesError);
            pipeline = false;
        } else {
            setError(location, "The return status of function " + functionCall + " has not been checked.", yyline+1-linesError);
        }
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	

%}

%eofval{
    if(!verified && functionCalled) addViolation();
	return getCheckResults();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); functions.put(location, false); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{location = yytext(); yybegin(NAMING);}
				{FUNC}			{location = yytext().substring(0,yytext().length()-2).trim(); functions.put(location, false);}
			    {STRING}		{}
			    {VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
			    				 		functionCall=yytext();
			    				 		yybegin(FINLINE);
			    				} else {
			    					functionCalled=false;
			    				}} 
			 	.              	{}
		}
		
/************************/
/* FINLINE STATE	    */
/************************/
<FINLINE>   	
		{
				\|				{pipeline = true; yybegin(PIPELINE);}
				\n				{yybegin(CHECKRET);}
			   	.              	{}
		}

/************************/
/* PIPELINE STATE	    */
/************************/
<PIPELINE>   	
		{
				{VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
			    				 		functionCall+=", " + yytext();
			    				}} 
				\n				{yybegin(CHECKRET);}
			   	.              	{}
		}
		
		
/************************/
/* CHECKRET STATE	    */
/************************/
<CHECKRET>   	
		{
				\#				{yybegin(COMMENTARGS);}
				{VAR}			{Boolean found = functions.get(yytext());
			    				 if(found!=null) {
			    				 		addViolation();
			    				 		functionCalled=true;
			    				 		verified=false;
			    				 		linesError=1;
			    				 		functionCall=yytext();
			    				 		yybegin(FINLINE);
			    				} else {
			    					functionCalled=false;
			    				}} 
				\$\?			{verified=true;}
				{SPACE}			{}
				\n				{if(!verified) addViolation();
								 functionCalled = false;
								 yybegin(YYINITIAL);}
			   	.              	{}
		}
		
/************************/
/* COMMENTARGS STATE	*/
/************************/
<COMMENTARGS>   	
		{
				\n				{linesError++; yybegin(CHECKRET);}
			   	.              	{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {}