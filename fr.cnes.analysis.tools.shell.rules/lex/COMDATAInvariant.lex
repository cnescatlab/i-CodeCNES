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

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInvariant
%extends AbstractChecker
%public
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, INVARIANT, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
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

CLE			 = "alias" | "apropos" | "apt-get" | "aptitude" | "ascp" | "aspell" | "awk" | 
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
	private String location = MAINPROGRAM;
	/* VARIABLE: constant for variable error message */
	private static final String VARIABLE = " -> The variable ";
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
	
	/* localErrVariables: contains all current function local variables that for the moment */
	/* should be consts */
	/* String: variable name, Integer: variable line in code */
	private final Map<String,Integer> localErrVariables = new HashMap<String,Integer>();
	/* localOkVariables: contains all current function variables that have either been declared as */
	/* consts or should be variables. There should be no violation on them. */
	private final List<String> localOkVariables = new ArrayList<String>();

	
	private boolean variableError = false;
	private boolean invariantError = false;
	private boolean separator = false;
	private String variable = "";
	

    /* addVar: method that adds the just initialised variable var to the correct list according to its status */ 
	private void addVar(final String var) {
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
		}
	} 

    /* localAddVar: method that adds the just initialised local variable var to the correct list according to its status */ 
	private void localAddVar(final String var) {
		final Boolean found = localErrVariables.containsKey(var);
		if (found) {
			/* var is in localErrVariables, this is the 2nd initialisation */
			/* var doesn't need to be const */
			localErrVariables.remove(var);
			localOkVariables.add(var);
		} else if (!localOkVariables.contains(var)) {
			/* var isn't in the already initiated variables in any way */
			/* this is its 1st initialisation */
			localErrVariables.put(var, yyline);
		}
	} 


    /* addViolationLocation: adds the list of violations on variables of the just ended function (location) */ 
    private void addViolationsLocation() throws JFlexException {
        for (final Map.Entry<String, Integer> entry : localErrVariables.entrySet()) {
            final String var = entry.getKey();
            final Integer line = entry.getValue();
            /* location is current location */
            setError(location, VARIABLE + var + DECLARE_CONST, line+1);
        }
        /* Clear the local tables ready for the next function */
        localErrVariables.clear();
        localOkVariables.clear();
	}

    public COMDATAInvariant() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
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
				{VAR}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD}  	{yybegin(COMMENT);}
				{FUNCTION}			{if (! location.equals(MAINPROGRAM)) addViolationsLocation(); yybegin(NAMING);}
				{FUNCT}				{if (! location.equals(MAINPROGRAM)) addViolationsLocation(); location = yytext().substring(0,yytext().length()-2).trim();}
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
				{LOCALREADONLY}{VAR}\= {int varPos = yytext().lastIndexOf(' ');
										String var = yytext().substring(varPos, yytext().length()-1);
										localOkVariables.add(var);
										yybegin(INVARIANT);}
				{LOCAL}{VAR}\=		{int varPos = yytext().lastIndexOf(' ');
									 String var = yytext().substring(varPos, yytext().length()-1);
									 localAddVar(var);
									 yybegin(INVARIANT);}
			 	.              		{}
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
/* ERROR STATE	        */
/************************/
				[^]            {}