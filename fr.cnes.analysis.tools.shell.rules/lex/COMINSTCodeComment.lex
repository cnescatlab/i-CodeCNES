/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.CodeComment rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTCodeComment
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, PREHEADER, HEADER, HEADER_2, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

RESERVED 	 = "case"	| "do"		| "done" 	| "elif"	|
			   "else"	| "esac"	| "fi"		| "for"		|
			   "function"| "if"		| "in"		| "select"	|
			   "then"	| "time"	| "until"	| "while"	
POSIX		= "admin"	| "alias"	| "ar"		| "asa"		|
			  "at"		| "awk"		| "basename"| "batch"	|	
			  "bc"		| "bg"		| "c99"		| "cal"		|
			  "cat"		| "cd"		| "cflow"	| "chgrp"	|
			  "chmod"	| "chown"	| "cksum"	| "cmp"		|
			  "comm"	| "command"	| "cp"		| "crontab"	|
			  "csplit"	| "ctags"	| "cut"		| "cxref"	|
			  "date"	| "dd"		| "delta"	| "df"		|
			  "diff"	| "dirname"	| "du"		| "echo"	|
			  "ed"		| "ed"		| "env"		| "ex"		|
			  "expand"	| "expr"	| "false"	| "fc"		|
			  "fd"		| "file"	| "find"	| "fold"	|
			  "fort77"	| "fuser"	| "gencat"	| "get"		|
			  "getconf"	| "getopt"	| "grep"	| "hash"	|
			  "head"	| "iconv"	| "id"		| "ipcrm"	|
			  "ipcs"	| "jobs"	| "join"	| "kill"	|
			  "lex"		| "link"	| "ln"		| "locale"	|
			  "localedef"| "logger"	| "logname"	| "lp"		|
			  "ls"		| "m4"		| "mailx"	| "make"	|
			  "man"		| "mesg"	| "mkdir"	| "mkfifo"	|
			  "more"	| "mv"		| "newgrp"	| "nice"	|
			  "nl"		| "nm"		| "nohup"	| "od"		|
			  "paste"	| "patch"	| "pathchk"	| "pax"		|
			  "pr"		| "printf"	| "prs"		| "pwd"		|
			  "qalter"	| "qdel"	| "qhold"	| "qmove"	|
			  "qmsg"	| "qrerun"	| "qrls"	| "qselect"	|
			  "qsig"	| "qsub"	| "read"	| "renice"	|
			  "rm"		| "rmdel"	| "rmdir"	| "sact"	|
			  "sccs"	| "sed"		| "sleep"	|
			  "sort"	| "split"	| "stat"	| "strings"	|
			  "strip"	| "stty"	| "tabs"	| "tail"	|
			  "talk"	| "tee"		| "test"	| "time"	|
			  "touch"	| "tput"	| "tr"		| "true"	|
			  "tsort"	| "tty"		| "type"	| "ulimit"	|
			  "umask"	| "unalias"	| "uname"	| "uncompress"|
			  "unexpand"| "unget"	| "uniq"	| "unlink"	|
			  "uucp"	| "uudecode"| "uuencode"| "uustat"	|
			  "uux"		| "val"		| "vi"		| "wait"	|
			  "wc"		| "what"	| "who"		| "write"	|
			  "xargs"	| "yacc"	| "zcat"
			  
BUILTINS	= "bind"	| "break"	| "builtin"	| "caller"	|
			  "continue"| "declare" | "enable"	| "eval"	|
			  "exec"	| "exit"	| "export"	| "getopts" |
			  "help"	| "let"		| "local"	| "mapfile"	|
			  "readarray"| "readonly"| "return"	| "set"		|
			  "shift"	| "shopt"	| "source"	| "times"	|
			  "trap"	| "typeset"	| "unset" 
CLE			= {RESERVED}| {POSIX} | {BUILTINS}


																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> loc = new LinkedList<String>();

    public COMINSTCodeComment() {
    	loc.add("MAIN PROGRAM");
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
			
%}

%eofval{
	return getCheckResults();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				{CLE}{SPACE}+	{setError(loc.get(loc.size()-1),"The keyword " + yytext() + " is used in a comment.", yyline+1); yybegin(AVOID);}
				{CLE}\n			{setError(loc.get(loc.size()-1),"The keyword " + yytext() + " is used in a comment.", yyline+1); yybegin(YYINITIAL);}
				{VAR}\=			{setError(loc.get(loc.size()-1),"A variable is assigned in a comment.", yyline+1); yybegin(AVOID);}
				{SPACE}	| \#	{}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{yybegin(AVOID);}
		}
		
		
/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID> 	  	\n             	{yybegin(YYINITIAL);}  
<AVOID>	   		.              	{}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); loc.add(yytext()); yybegin(PREHEADER);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{if(loc.size()>0) yybegin(COMMENT); else yybegin(PREHEADER);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); loc.add(location); yybegin(PREHEADER);}
			    {STRING}		{}
	      		[^]           	{}
		}


/************************/
/* HEADER STATE    	    */
/************************/
<PREHEADER>		
		{
				{COMMENT_WORD}	{yybegin(HEADER);}
				{FUNCTION}     	{location = yytext(); yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); loc.add(location); yybegin(NAMING);}
				{SPACE} | \n	{}
				.				{yybegin(YYINITIAL);}
		}
		
<HEADER>
		{		
			\n				{yybegin(HEADER_2);}
			.				{}
		}
		
<HEADER_2>
       {		
       		{COMMENT_WORD}	{yybegin(HEADER);}
			{FUNCTION}     	{location = yytext(); yybegin(NAMING);}
			{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); loc.add(location); yybegin(NAMING);}
			[^]				{yybegin(YYINITIAL);}
		}
		


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}