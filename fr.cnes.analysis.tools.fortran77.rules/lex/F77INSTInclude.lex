/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.INST.Incude rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

/* Column counting is set to help find line continuation. */
%class F77INSTInclude
%extends AbstractChecker
%public
%line
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/*
 * STATES
 */
%state COMMENT, NAMING, NEW_LINE, LINE, INCLUDE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
INCLUDE		 = INCLUDE	  | include
EXEC_STMNT   = ASSIGN     | assign	   | GO[\ ]+TO		| go[\ ]+to		|
			   IF		  | if		   | IF[\ ]+ELSE    | if[\ ]+else   |
			   ELSE		  | else	   | CONTINUE       | continue      |
			   STOP       | stop	   | PAUSE			| pause			|
			   DO		  | do		   | READ			| read			|
			   WRITE	  | write	   | PRINT			| print			|
			   REWIND	  | rewind	   | BACKSPACE		| backspace		|
			   ENDFILE	  | endfile	   | OPEN			| open			|
			   CLOSE	  | close	   | INQUIRE		| inquire		|
			   CALL		  | call	   | RETURN			| return
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

%{
	String location = "MAIN PROGRAM";
	
	boolean include = false;
	private String fileName ;
	private File includeFile;
	private String project; 
	/** name of the file parsed */
	private String parsedFileName;
	
	public F77INSTInclude() {
		this.include = false;
    }
    
    public F77INSTInclude(boolean included, IConfigurationElement pContribution) {
		this.setContribution(pContribution);
		this.include = included;
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		project = getProjectPath(new Path(file.getAbsolutePath()).toOSString());
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void analyseFile(String fileN) throws JFlexException {
		fileName = fileN.replaceAll("\\'", "");
		if(fileName.contains("/")) {
			String [] str = fileName.split("/");
			fileName = str[str.length-1];
		}
		try {
			final AbstractChecker rule = new F77INSTInclude(true, this.getContribution());

			File[] currDir = new File(project).listFiles();
			getFileFromPath(currDir);
			if (includeFile != null) {
				rule.setInputFile(includeFile);
	
				// We verify that there is an error.
				List<CheckResult> l = rule.run();
				if (!l.isEmpty()) {
					CheckResult fv = l.get(0);
					if (fv.getLine() != 0)	setError(location,"There is an executable instruction not allowed in the included file.", yyline + 1);
				}
			}
		} catch (FileNotFoundException exception) {
			throw new JFlexException(exception);
		} catch (IOException exception) {
			throw new JFlexException(exception);
		} catch (JFlexException exception) {
			throw new JFlexException(exception);
		}
	}
	
	private String getProjectPath(String filePath) {
		String sep = File.separator.equals("/") ? "/" : "\\\\";
 		String str[]= filePath.split(sep);
		String ret = "";
		for(int i = 0; i < str.length -1; i++) {
			ret = ret + str[i] + File.separator;
		}
		return ret;
	}
	
	private void getFileFromPath(File[] files) {
		for (File file : files) {
	        if (file.isDirectory()) {
	            getFileFromPath(file.listFiles());
	        } else {
	            if(file.getName().equals(fileName)) {
	            	includeFile = file;
	            }
	        }
	    }
	}
%}

%eofval{
return getCheckResults();
%eofval}


%%          
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/
				{FREE_COMMENT}	{yybegin(COMMENT);}  

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}					 

/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{INCLUDE}		{yybegin(INCLUDE);}
<YYINITIAL>		{EXEC_STMNT}	{if(include) setError(location,"The executable instruction " + yytext() + " is not allowed in the include file.",yyline+1);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/*********************/
/*	NEW LINE STATE	 */
/*********************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{INCLUDE}		{yybegin(INCLUDE);}
<NEW_LINE>		{EXEC_STMNT}	{if(include) setError(location ,"The executable instruction " + yytext() + " is not allowed in the include file.",yyline+1);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

/*****************/
/*	LINE STATE	 */
/*****************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{INCLUDE}		{yybegin(INCLUDE);}
<LINE>			{EXEC_STMNT}	{if(include) setError(location,"The executable instruction " + yytext() + " is not allowed in the include file.",yyline+1);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

/*********************/
/*	INCLUDE STATE	 */
/*********************/
<INCLUDE>		{STRING}		{if(yytext().contains(".f") || yytext().contains(".F")) analyseFile(yytext());}
<INCLUDE>		\n				{yybegin(NEW_LINE);}
<INCLUDE>		.				{}

/*********************/
/*	ERROR THROWN	 */
/*********************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}