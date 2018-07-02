/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Invariant rule. */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.EmptyStackException;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class COMDATAInvariant
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, INVARIANT, AVOID, BEGINFUNC, AWK, AWK_STRING, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
FUNCT			= {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FUNCTION    	= "function"
FNAME		 	= [a-zA-Z0-9\.\!\-\_\@\?\+]+
NAME	     	= [a-zA-Z\_][a-zA-Z0-9\_]*
SPACE			= [\ \r\t\f\space]*
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
EXTENSION	 = (\.{VAR})+
SEPAR		 = [\ ]	| \+	| \-	| \*	| \/
INT			 = [0-9]+

PATH 		 = (\/)? ({VAREXT}\/)* {VAREXT}
VAREXT		 = {VARP}	| \${VARP}	| \$\{{VARP}\}
VARP		 = [a-zA-Z][a-zA-Z0-9\_\.\-]*

OPTIONS		 = \-({VAR}|{INT})	| {INT}(\>)+

OPT			 = "-"[a-zA-Z]
OPTR		 = "-"[a-qs-zA-Z]
READONLY	 = "readonly" {SPACE}+ ({OPT}{SPACE}+)* | ("declare"|"typeset") {SPACE}+ ({OPTR}{SPACE}+)* "-r" ({SPACE}+{OPTR})* {SPACE}+ 
LOCAL		 = "local" {SPACE}+ ({OPTR}{SPACE}+)*
LOCALREADONLY = "local" {SPACE}+ ({OPTR}{SPACE}+)* "-r" ({SPACE}+{OPTR})* {SPACE}+ 

OPER		 =  "++" | "--"

IGNORE		 = "<<" {SPACE}* "EOF" [^"<<"]* "EOF" | "typeset" | "declare" | "--"[a-zA-Z\-]*"="

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "done"

AWK			 = "awk"
COMMAND_END	 = [\n;]

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

CLE			 = "alias" | "apropos" | "apt-get" | "aptitude" | "ascp" | "aspell" |
			   "basename" | "bash" | "bc" | "bg" | "break" | "builtin" | "bzip2" | 
			   "cal" | "case" | "cd" | "cfdisk" | "chgrp" | "chmod" | "chown" | 
			   "chroot" | "chkconfig" | "cksum" | "clear" | "cmp" | "comm" | "command" | 
			   "continue" | "cp" | "cron" | "crontab" | "csplit" | "cut" | "date" | 
			   "dc" | "dd" | "ddrescue" | "df" | "diff" | "diff3" | "dig" | 
			   "dir" | "dircolors" | "dirname" | "dirs" | "dmesg" | "du" | "echo" | 
			   "egrep" | "eject" | "enable" | "env" | "ethtool" | "exec" | 
			   "exit" | "expect" | "expand" | "expr" | "false" | "fdformat" | 
			   "fdisk" | "fg" | "fgrep" | "file" | "find" | "fmt" | "fold" | "for" | 
			   "format" | "free" | "fsck" | "ftp" | "fuser" | "gawk" | 
			   "getopts" | "grep" | "groupadd" | "groupdel" | "groupmod" | "groups" | 
			   "gzip" | "hash" | "head" | "help" | "history" | "hostname" | "iconv" | 
			   "id" | "if" | "ifconfig" | "ifdown" | "ifup" | "import" | "install" | 
			   "jobs" | "join" | "kill" | "killall" | "less" | "link" | "ln" | 
			   "locate" | "logname" | "logout" | "look" | "lpc" | "lpr" | 
			   "lprint" | "lprintd" | "lprintq" | "lprm" | "ls" | "lsof" | "make" | 
			   "man" | "mkdir" | "mkfifo" | "mkisofs" | "mknod" | "more" | "most" | 
			   "mount" | "mtools" | "mtr" | "mv" | "mmv" | "netstat" | "nice" | "nl" | 
			   "nohup" | "notify-send" | "nslookup" | "open" | "op" | "passwd" | 
			   "paste" | "pathchk" | "ping" | "pkill" | "popd" | "pr" | "printcap" | 
			   "printenv" | "printf" | "ps" | "pushd" | "pv" | "pwd" | "quota" | 
			   "quotacheck" | "quotactl" | "ram" | "rcp" | "read" | "readarray" | 
			   "readonly" | "reboot" | "rename" | "renice" | "remsync" | "return" | 
			   "rev" | "rm" | "rmdir" | "rsync" | "screen" | "scp" | "sdiff" | "sed" | 
			   "select" | "seq" | "set" | "sftp" | "shift" | "shopt" | "shutdown" | 
			   "sleep" | "slocate" | "sort" | "source" | "split" | "ssh" | "stat" | 
			   "strace" | "su" | "sudo" | "sum" | "suspend" | "sync" | "tail" | "tar" | 
			   "tee" | "test" | "time" | "timeout" | "times" | "touch" | "top" | 
			   "traceroute" | "trap" | "tr" | "true" | "tsort" | "tty" | "type" | 
			   "ulimit" | "umask" | "umount" | "unalias" | "uname" | "unexpand" | "uniq" | 
			   "units" | "unset" | "unshar" | "until" | "uptime" | "useradd" | "userdel" | 
			   "usermod" | "users" | "uuencode" | "uudecode" | "vdir" | "vi" | 
			   "vmstat" | "wait" | "watch" | "wc" | "whereis" | "which" | "while" | "who" | 
			   "whoami" | "wget" | "write" | "xargs" | "xdg-open" | "yes" | "zfs" | "zip"

																
%{
    /* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
    private String parsedFileName;
	private String location = MAINPROGRAM;
	/* VARIABLE: constant for variable error message */
	private static final String VARIABLE = "The variable ";
	/* DECLARE_CONST: message for constant error message */
	private static final String DECLARE_CONST = " should be declared constant";
	/* errVariables: contains all variables that for the moment should be consts */
	/* String: variable name, Integer: variable line in code */
	private final Map<String,Integer> errVariables = new HashMap<String,Integer>();
	/* okVariables: contains all variables that have either been declared as consts or */
	/* should be variables. There should be no violation on them. */
	private final List<String> okVariables = new ArrayList<String>();
	/* varLocations: contains the location of the variables, according to their code line number*/
	/* int: variable line in code, String: location in code (function name) */
	private final Map<Integer,String> varLocations = new HashMap<Integer,String>();
	
	List<String> globalVariables = new ArrayList<String>();
	
	private boolean variableError = false;
	private boolean invariantError = false;
	private boolean separator = false;
	private String variable = "";
	
 	/* functionLine: the beginning line of the function */
	int functionLine = 0;

	/* The global variables in the class will be used to stock the okVariables of the function */
	private Stack<FunctionInvariant> functionStack = new Stack<>();
	

    /* addVar: method that adds the just initialised variable var to the correct list according to its status */ 
	private void addVar(final String var) {	
       if(!functionStack.empty()){
			/* we are in a function: add the variable to the correct list in the function */
		    functionAddVar(var);
        } else {
			/* we are in main */
			mainAdd(var);
		}
	}

    /* mainAdd: method that adds the just initialised variable var to the correct list according to its status */ 
	/* we are in main */
	private void mainAdd(final String var) {
		final Boolean found = errVariables.containsKey(var);
		if (found) {
			/* var is in errVariables, this is the 2nd initialisation */
			/* var doesn't need to be const */
			errVariables.remove(var);
			okVariables.add(var);
		} else if (!okVariables.contains(var)) {
			/* var isn't in the already initiated variables in any way */
			/* this is its 1st initialisation */
			errVariables.put(var, yyline);
			varLocations.put(yyline, location);
			globalVariables.add(var);
		}
	} 
	
    /* functionAddVar: method that adds the just initialised variable var to the correct list according to its status */ 
	/* we are in a function */
	private void functionAddVar(final String var) {
		FunctionInvariant function = functionStack.peek();
		HashMap<String,Integer> functionErrVariables = function.getErrVariables();
		final Boolean found = functionErrVariables.containsKey(var);
		List<String> functionOkVariables = function.getOkVariables();
		List<String> functionGlobals = function.getGlobalVariables();
		List<String> functionOkGlobalVariables = function.getOkGlobalVariables();
		
		if (found) {
			/* var is in function ErrVariables, this is the 2nd initialisation */
			/* var doesn't need to be const */
			functionErrVariables.remove(var);
			functionOkVariables.add(var);
		} else if (!functionOkVariables.contains(var)) {
			/* var isn't in the already initiated local variables in any way */
			/* this is its 1st initialisation */
			if (!functionGlobals.contains(var))
			{
				/* the variable is not global: that means it is really a 1st local initialisation */
				function.getLocalVariables().add(var);
				functionErrVariables.put(var, yyline);
			} else {
				/* the variable is global for the function, meaning that this is its 2nd initialisation */
				/* the functionOkVariables will be passed up to the containing function or main */
				functionOkVariables.add(var);
				functionOkGlobalVariables.add(var);
			}
		}
	}
	

    /* localAddVar: method that adds the just initialised local variable var to the correct list according to its status */ 
	/* always called in the case of a local variable */
	private void localAddVar(final String var, Boolean localReadOnly) {
 		FunctionInvariant function = functionStack.peek();
		HashMap<String,Integer> functionErrVariables = function.getErrVariables();
		final Boolean found = functionErrVariables.containsKey(var);
		List<String> functionOkVariables = function.getOkVariables();
		
		if (localReadOnly) {
			functionOkVariables.add(var);
			function.getLocalVariables().add(var);
		} else if (found) {
			/* var is in function ErrVariables, this is the 2nd initialisation */
			/* var doesn't need to be const */
			functionErrVariables.remove(var);
			functionOkVariables.add(var);
		} else if (!functionOkVariables.contains(var)) {
			/* var isn't in the already initiated variables in any way */
			/* this is its 1st initialisation */
			function.getLocalVariables().add(var);
			functionErrVariables.put(var, yyline);
		}		
	} 

    /* addViolationLocation: adds the list of violations on variables of the just ended function (location) */
	/* Called at the end of a function */
    private void addViolationsLocation(HashMap<String,Integer> functionErrVariables) throws JFlexException {
        for (final Map.Entry<String, Integer> entry : functionErrVariables.entrySet()) {
            final String var = entry.getKey();
            final Integer line = entry.getValue();
            /* location is current location */

            setError(location, VARIABLE + var + DECLARE_CONST, line+1);
        }
	}

    public COMDATAInvariant() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void endLocation() throws JFlexException {
		try{
		    FunctionInvariant functionFinished = functionStack.pop();
			addViolationsLocation(functionFinished.getErrVariables());
			/* list of son function's locals, that also contain relative global OKs */
			/* Remove those that were locals first */
			ArrayList<String> sonOkVariables = functionFinished.getOkGlobalVariables();
			if (!functionStack.empty()) {
				/* there is a current function */
				FunctionInvariant currentFunction = functionStack.peek();
				currentFunction.addSonOkVariables(sonOkVariables);
				location = currentFunction.getName();
			} else {
				/* we are in the main program */
				for (String var : sonOkVariables) {
					errVariables.remove(var);
					okVariables.add(var);
				}
				location = MAINPROGRAM;			
			}
		}catch(EmptyStackException e){
        		final String errorMessage = e.getMessage();
            	throw new JFlexException(this.getClass().getName(), parsedFileName,
										 errorMessage, yytext(), yyline, yycolumn);
		}
	}
	
		/** 
     * setGlobals: adds the current globals to the globals of pFunction.
	 * If there is a higher level function, its locals are also added.
     */
	private void setGlobals(FunctionWithVariables pFunction) throws JFlexException {
       if(!functionStack.empty()){
			/* we are in a function: add the locals of the current function as globals of the new function */
		    pFunction.getGlobalVariables().addAll(functionStack.peek().getGlobalVariables());
		    pFunction.getGlobalVariables().addAll(functionStack.peek().getLocalVariables());
       } else {
			pFunction.getGlobalVariables().addAll(globalVariables);	
		}
	}    

%}

%eofval{
    for (final Map.Entry<String, Integer> entry : errVariables.entrySet()) {
        final String var = entry.getKey();
        final Integer line = entry.getValue();
        location = varLocations.get(line);
        setError(location, VARIABLE + var + DECLARE_CONST, line+1);
    }
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
/* AVOID STATE	    	*/
/************************/
<AVOID>   	
		{
				\\ {SPACE}* \n	{}
				\n | \;        	{yybegin(YYINITIAL);}
			   	.              	{}
		}
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(BEGINFUNC);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  	{yybegin(COMMENT);}
				{AWK}				{yybegin(AWK);}
				{FUNCTION}			{yybegin(NAMING);}
				{FUNCT}				{location = yytext().substring(0,yytext().length()-2).trim();
									 yybegin(BEGINFUNC);}
				{FUNCSTART}		{
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									} 
		      					}
	      		{FUNCEND}		{
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}
		      					}
			    /** variables intialisation -> new sate to check rule **/
			    {CLE}{SPACE}+		{yybegin(AVOID);}
				{IGNORE}			{}
				{READONLY}{VAR}\=	{int varPos = yytext().lastIndexOf(' ');
									 String var = yytext().substring(varPos, yytext().length()-1);
									 okVariables.add(var); 
									 yybegin(INVARIANT);}
			    {VAR}\=				{addVar(yytext().substring(0, yytext().length()-1)); yybegin(INVARIANT);}
				{SPACE}{1}{VAR}{OPER} {addVar(yytext().substring(1, yytext().length()-2)); yybegin(INVARIANT);}
				{OPER}{VAR}{SPACE}{1} {addVar(yytext().substring(2, yytext().length()-1)); yybegin(INVARIANT);}
				{VAR}"+="			{addVar(yytext().substring(0, yytext().length()-2)); yybegin(INVARIANT);}
				{LOCALREADONLY}{SPACE}{VAR}\= {
										int varPos = yytext().lastIndexOf(' ') + 1;
										String var = yytext().substring(varPos, yytext().length()-1);
										localAddVar(var, true);
										yybegin(INVARIANT);}
				{LOCAL}{SPACE}{VAR}\= {
									 int varPos = yytext().lastIndexOf(' ') + 1;
									 String var = yytext().substring(varPos, yytext().length()-1);
									 localAddVar(var, false);
									 yybegin(INVARIANT);}
	 			[^]              	{}
		}
		
/************************/
/* INVARIANT STATE	    */
/************************/
<INVARIANT>
		{
			  	{COMMENT_WORD}  				{variableError=false; separator=false; invariantError=false; yybegin(COMMENT);}
				` [^`]* `						{}
				{CLE}							{variableError=false; separator=false; invariantError=false; yybegin(AVOID);}
				{OPTIONS}						{}
				\${VAR}		 					{variableError=true;}
			    {SEPAR}							{if(variableError || invariantError)separator=true;}
			    {VAR} | {INT} | {EXTENSION}		{invariantError=true; variable=yytext();}
			    {PATH}							{}
			    \|	| \; | \|\| |"&&"			{variableError=false; separator=false; invariantError=false; yybegin(YYINITIAL);}
			    {SPACE}*\n						{if(variableError && invariantError && separator) setError(location, "The literal " + variable + " should be defined as a constant.", yyline+1);
			    					 			 variableError=false; separator=false; invariantError=false; yybegin(YYINITIAL);}
			 	.              					{}
		}

/************************/
/* BEGINFUNC STATE	    */
/************************/
/*
 * This state target is to retrieve the function starter. For more information on fonction starter, have a look on {@link Function} class.
 * Pending this starter, the function ender can be defined.
 *
 */ 
<BEGINFUNC>
		{
				\(\)			{}
				{FUNCSTART}		{
									FunctionInvariant function;
									function = new FunctionInvariant(location, functionLine, yytext());
									setGlobals(function);
									functionStack.push(function);
								 	yybegin(YYINITIAL);
							 	}
			   	[^]|{SPACE}  {}
		}

/*
 * The string states are designed to avoid problems due to patterns found in strings.
 */ 
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
				{IGNORE_STRING_S}	{}
				{STRING_S}    		{yybegin(YYINITIAL);}  
		  	 	[^]|{SPACE}  		{}
		}

/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
				{IGNORE_STRING_D}	{}
				{STRING_D}    		{yybegin(YYINITIAL);}  
		  	 	[^]|{SPACE}  		{}
		}
/*
 * The AWK states are designed to ignore awk commands
 */ 
/************************/
/* AWK STATE	    */
/************************/
<AWK>   	
		{
				{STRING_S}    		{yybegin(AWK_STRING);}  
				{COMMAND_END}  		{yybegin(YYINITIAL);}
				.					{}
		}

/************************/
/* AWK_STRING STATE	    */
/************************/
<AWK_STRING>   	
		{
				{IGNORE_STRING_S}	{}
				{STRING_S}    		{yybegin(AWK);}  
		  	 	[^]|{SPACE}  		{}
		}
	

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}