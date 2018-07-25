/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for SH.INST.POSIX rule.	 		  */
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
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
%%

%class SHINSTPOSIX
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, STRING, ECHO

/********************************************************************************************************/
/* This rule consist on throw an error when an Bash command is found and it is not a POSIX command.		*/
/* POSIX command are listed in the rule SH.INST.POSIX in the RNC (ex: admin, alias, ar, asa, ...)		*/
/* All the bash commands are listed in: http://ss64.com/bash/											*/
/********************************************************************************************************/


COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACE}*[\(]{SPACE}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+
SPACE		 = [\ \r\t\f\space]
VAR			 = ([a-zA-Z][a-zA-Z0-9\_\-]*)|([\$]([\-\@\?\#\!\_\*]|([a-zA-Z0-9]*)|[\{][a-zA-Z0-9]*[\}]))
ECHO		 = "echo"
END_ECHO	 = [\;]|[\|]|[\>]|([\&][\&])

STRING 		 = [\']|[\"]
ESCAPED_STRING = [\\]([\']|[\"])


ERROR		 = "apropos" |"apt-get" |"aptitude" |"aspell" |"bash" |
			   "bc" |"break" |"builtin" |"bzip2" |"cfdisk" |"chroot" |
			   "chkconfig" |"clear" |"cron" |"dc" |"ddrescue" |
			   "declare" |"diff3" |"dig" |"dir" |"dircolors" |"dirs" |"dmesg" |
			   "egrep" |"eject" |"enable" |"ethtool" |"eval" |"exec" |"exit" |
			   "expect" |"export" |"fdformat" |"fdisk" |"fg" |"fgrep" |
			   "fmt" |"format" |"free" |"fsck" |"ftp" |"function" |"gawk" |
			   "groupadd" |"groupdel" |"groupmod" |"groups" |"gzip" |"help" |
			   "history" |"hostname" |"ifconfig" |"ifdown" |"ifup" |"import" |
			   "install" |"killall" |"less" |"let" |"local" |"locate" |"logout" |
			   "look" |"lpc" |"lpr" |"lprint" |"lprintd" |"lprintq" |"lprm" |"lsof" |
			   "mkisofs" |"mknod" |"most" |"mount" |"mtools" |"mtr" |"mmv" |"netstat" |
		   	   "notify-se" |"nslookup" |"open" |"passwd" |"ping" |"pkill" |"popd" |
		   	   "printcap" |"printenv" |"ps" |"pushd" |"pv" |"quota" |"quotachec" |
		   	   "quotactl" |"ram" |"rcp" |"readarray" |"readonly" |"reboot" |"rename" |
		   	   "remsync" |"rev" |"rsync" |"screen" |"scp" |"sdiff" |"select" |
		   	   "seq" |"sftp" |"shopt" |"shutdown" |"slocate" |"source" |
   	   		   "ssh" |"strace" |"su" |"sudo" |"sum" |"suspend" |"sync" |"tar" |"timeout" |
   	   		   "times" |"top" |"tracerout" |"trap" |"umount" |"units" |"unset" |"unshar" |
   	   		   "uptime" |"useradd" |"userdel" |"usermod" |"users" |"vdir" |
   	   		   "vmstat" |"watch" |"whereis" |"which" |"whoami" |"wget" |"xdg-open" |"yes" |
   	   		   "zip" 




NOT_ERROR	= (([a-zA-Z0-9\_\-]+{ERROR}) | ({ERROR}[a-zA-Z0-9\_\-]+)) | (([\\]|[\/]){ERROR}) | ({ERROR}([\\]|[\/])) | ([\.]{ERROR})

COMMAND = ([\$][\(].+[\)]) | ([\`].+[\`])
VAR_ERROR   = ([\$]{ERROR}) | ([\$][\{]{ERROR}[\}])

																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	List<String> variables = new ArrayList<String>();
	/** The next char must be escaped*/
	private boolean escapeNext = false;
	private String stringBeginner = "";
	private boolean echoContainString = false;
	
	private ArrayList<String> keywords = new ArrayList<>(Arrays.asList("apropos" ,"apt-get" ,"aptitude" ,"aspell" ,"bash" ,
			   "bc" ,"break" ,"builtin" ,"bzip2" ,"cfdisk" ,"chroot" ,
			   "chkconfig" ,"clear" ,"cron" ,"dc" ,"ddrescue" ,
			   "declare" ,"diff3" ,"dig" ,"dir" ,"dircolors" ,"dirs" ,"dmesg" ,
			   "egrep" ,"eject" ,"enable" ,"ethtool" ,"eval" ,"exec" ,"exit" ,
			   "expect" ,"export" ,"fdformat" ,"fdisk" ,"fg" ,"fgrep" ,
			   "fmt" ,"format" ,"free" ,"fsck" ,"ftp" ,"function" ,"gawk" ,
			   "groupadd" ,"groupdel" ,"groupmod" ,"groups" ,"gzip" ,"help" ,
			   "history" ,"hostname" ,"ifconfig" ,"ifdown" ,"ifup" ,"import" ,
			   "install" ,"killall" ,"less" ,"let" ,"local" ,"locate" ,"logout" ,
			   "look" ,"lpc" ,"lpr" ,"lprint" ,"lprintd" ,"lprintq" ,"lprm" ,"lsof" ,
			   "mkisofs" ,"mknod" ,"most" ,"mount" ,"mtools" ,"mtr" ,"mmv" ,"netstat" ,
		   	   "notify-se" ,"nslookup" ,"open" ,"passwd" ,"ping" ,"pkill" ,"popd" ,
		   	   "printcap" ,"printenv" ,"ps" ,"pushd" ,"pv" ,"quota" ,"quotachec" ,
		   	   "quotactl" ,"ram" ,"rcp" ,"readarray" ,"readonly" ,"reboot" ,"rename" ,
		   	   "remsync" ,"rev" ,"rsync" ,"screen" ,"scp" ,"sdiff" ,"select" ,
		   	   "seq" ,"sftp" ,"shopt" ,"shutdown" ,"slocate" ,"source" ,
   	   		   "ssh" ,"strace" ,"su" ,"sudo" ,"sum" ,"suspend" ,"sync" ,"tar" ,"timeout" ,
   	   		   "times" ,"top" ,"tracerout" ,"trap" ,"umount" ,"units" ,"unset" ,"unshar" ,
   	   		   "uptime" ,"useradd" ,"userdel" ,"usermod" ,"users" ,"vdir" ,
   	   		   "vmstat" ,"watch" ,"whereis" ,"which" ,"whoami" ,"wget" ,"xdg-open" ,"yes" ,
   	   		   "zip" ));
	
    public SHINSTPOSIX() {
    	
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
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{FNAME}			{location = yytext(); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
			  	{ESCAPED_STRING} { }
			  	{STRING}		{stringBeginner = yytext(); yybegin(STRING);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {ECHO}			{
									yybegin(ECHO);
								}
				
				{COMMAND} 		{
									for(String k : keywords){
										Pattern regex = Pattern.compile("(?<![a-zA-Z0-9\\.\\-\\\\\\/\\_])"+k+"(?![a-zA-Z0-9\\.\\-\\\\\\/\\_])");
										Matcher matchK = regex.matcher(yytext()); 
										if(matchK.find()){
											setError(location,"The keyword "+k+" is not allowed.", yyline+1);
										}
									}
								}
				{NOT_ERROR} 	{ if(this.keywords.contains(yytext())){
									setError(location,"The keyword " + yytext() + " is not allowed.", yyline+1);
								  }
								}
			    {ERROR}			{setError(location,"The keyword " + yytext() + " is not allowed.", yyline+1);}
	      		[^]         	{}
		}
		
/************************/
/* STRING STATE	    */
/************************/
<STRING>   	
		{
				{COMMAND} 		{
									escapeNext = false; 
									for(String k : keywords){
										Pattern regex = Pattern.compile("(?<![a-zA-Z0-9\\.\\-\\\\\\/\\_\\$])"+k+"(?![a-zA-Z0-9\\.\\-\\\\\\/\\_])");
										Matcher matchK = regex.matcher(yytext()); 
										if(matchK.find()){
											setError(location,"The keyword "+k+" is not allowed.", yyline+1);
										}
									}
									
									
								}
				{VAR_ERROR}	{	
									escapeNext = false; 
									setError(location,"The keyword " + yytext().replaceAll("\\$","").replaceAll("\\(", "") + " is not allowed.", yyline+1);
							}

			    
				{STRING}	{if(!escapeNext && stringBeginner.equals(yytext())){
									if(echoContainString){
										echoContainString=false;
										yybegin(ECHO);
									}else{
										yybegin(YYINITIAL);
									}
								 }else{
								 	escapeNext = false;
								 }
								}
				[\\]			{ escapeNext = true; }
			   	.              	{ escapeNext = false;
			   						
			   					}
			   	\n				{
			   					  escapeNext = false;
			   					}
		}
/************************/
/* ECHO STATE	    */
/************************/
<ECHO>   	
		{
				{ESCAPED_STRING}			{ }
				{STRING}					{ if(!escapeNext){
													stringBeginner = yytext();
													echoContainString=true;
													yybegin(STRING);
												  }else{
												    escapeNext = false;
												  }
											}
				{END_ECHO}	{
												escapeNext = false;
												yybegin(YYINITIAL);
											}
				\n								{
												escapeNext = false;
												yybegin(YYINITIAL);
											}
				[\\]						{	
												escapeNext = true;
											}
				.							{ 	
												escapeNext = false;
											}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}