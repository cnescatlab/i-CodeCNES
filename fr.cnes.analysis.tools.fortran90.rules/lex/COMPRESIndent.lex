/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Indent rule.	    */
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESIndent
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, IF_STATE, TYPE_STATE, DO_STATE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \! [^\n]*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTER		 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTER} |
			   {DATA_TYPE2} ([\ ]+"RECURSIVE")? [\ ]+ {FUNC}		|
			   {DATA_TYPE2}[\ ]*\([^\)]+\)[\ ]*{FUNC}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
DATA_TYPE2 	 = {DATA_TYPE} 	| "TYPE"
INSTRUCT	 = "DO"      | "IF"      | "WHILE"	  | 
			   "WHERE"   | "SELECT"  | "TYPE"
INST		 = {INSTRUCT}[\ ] | {INSTRUCT}\( | {INSTRUCT}\, | {INSTRUCT}\n | {INSTRUCT}\r
ELSE		 = "ELSE"    | "ELSE"[\ ]*"IF"
END			 = "END"
END_INST	 = {END}[\ ]*{INSTRUCT}	| {END}\n	| {END}[\ ]&&!({END}[\ ]*\=) |  {END}[\ ]&&!({END}[\ ]*\,)
END_TYPE	 = {END}[\ ]*{TYPE}
INCLUDE		 = "INCLUDE "
THEN		 = "THEN"
CONTINUE	 = "CONTINUE"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
INT			 = [0-9]+
SPACE		 = [\ \r\t\f]
																
%{
	String location = "MAIN PROGRAM";
	int currentCol = 0;
	List<Integer> theoricCol = new LinkedList<Integer>();
	List<Integer> numbers    = new LinkedList<Integer>();
	boolean funct = false;
	boolean endLine = true;
	int lastNum = 0;
	
	public COMPRESIndent(){
		theoricCol.add(0);
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	

	
%}

%eofval{

	return getCheckResults();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{if(endLine) { currentCol=0; yybegin(NEW_LINE);}} 
<COMMENT>		&				{endLine=false;} 
<COMMENT>		{SPACE}			{}
<COMMENT>   	.              	{endLine=true;}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{ currentCol+=yytext().length(); 
								 location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{currentCol = 0;yybegin(NEW_LINE);}
<NAMING>		{SPACE}			{currentCol+=yytext().length(); }
<NAMING>    	.              	{currentCol+=yytext().length();}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{theoricCol.add(0); location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{currentCol=0; currentCol+=yytext().length(); yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{currentCol+=yytext().length(); yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{INT}			{currentCol+=yytext().length(); lastNum = Integer.parseInt(yytext());}
<NEW_LINE>		{TYPE}        	{theoricCol.add(0); numbers.clear(); location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{INCLUDE}		{yybegin(COMMENT);}
<NEW_LINE>		{DATA_TYPE}		{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); yybegin(COMMENT);}
<NEW_LINE>		{INST}			{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
								 if(yytext().toLowerCase().contains("if")) { yybegin(IF_STATE); }
								 else if(yytext().toLowerCase().contains("type")) {
								 	if(yytext().contains("(")) yybegin(COMMENT);
								 	else yybegin(TYPE_STATE);
								 }
								 else if(yytext().toLowerCase().contains("do")) {
								 	theoricCol.add(currentCol); 
								 	if(yytext().contains("\n"))yybegin(NEW_LINE);
								 	yybegin(DO_STATE);
								 }
								 else { theoricCol.add(currentCol); 
								 	if(yytext().contains("\n"))yybegin(NEW_LINE);
								 	else yybegin(COMMENT);
								 } }
<NEW_LINE>		{ELSE}			{if((theoricCol.size() > 0) && currentCol!=theoricCol.get(theoricCol.size()-1)) { setError(location,"The code is not indented.", yyline+1); }
                                 yybegin(COMMENT);}
<NEW_LINE>		{END_TYPE}		{if(theoricCol.size() > 0)theoricCol.remove(theoricCol.size()-1);
                                 yybegin(COMMENT);}
<NEW_LINE>		{END_INST}		{if((theoricCol.size() > 0) && theoricCol.get(theoricCol.size()-1) !=0 && currentCol!=theoricCol.get(theoricCol.size()-1)) { setError(location,"The code is not indented.", yyline+1); }
                                 if(theoricCol.size() > 0) theoricCol.remove(theoricCol.size()-1);
                                 yybegin(COMMENT);}
<NEW_LINE>		{CONTINUE}		{if(numbers.contains(lastNum)) {numbers.remove(numbers.indexOf(lastNum)); theoricCol.remove(theoricCol.size()-1); }
								 if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1);
								 yybegin(COMMENT);}
<NEW_LINE>		#				{yybegin(COMMENT);}
<NEW_LINE>		{VAR}			{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
                                 yybegin(AVOID);}
<NEW_LINE>  	\n             	{currentCol=0;}
<NEW_LINE>		{SPACE}			{currentCol+=yytext().length(); yybegin(LINE);}
<NEW_LINE>  	.              	{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
                                 yybegin(AVOID);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{}
<LINE>			{INT}			{currentCol+=yytext().length(); lastNum = Integer.parseInt(yytext());}
<LINE>			{TYPE}        	{theoricCol.add(0); numbers.clear(); location = yytext(); yybegin(NAMING);}
<LINE>			{INCLUDE}		{yybegin(COMMENT);}
<LINE>			{DATA_TYPE}		{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); yybegin(COMMENT);}
<LINE>			{INST}			{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
								 if(yytext().toLowerCase().contains("if")) { yybegin(IF_STATE); }
								 else if(yytext().toLowerCase().contains("type")) {
								 	if(yytext().contains("(")) yybegin(COMMENT);
								 	else yybegin(TYPE_STATE);
								 }
								 else if(yytext().toLowerCase().contains("do")) {
								 	theoricCol.add(currentCol); 
								 	if(yytext().contains("\n"))yybegin(NEW_LINE);
								 	yybegin(DO_STATE);
								 }
								 else { theoricCol.add(currentCol); 
								 	if(yytext().contains("\n"))yybegin(NEW_LINE);
								 	else yybegin(COMMENT);
								 } }
<LINE>			{ELSE}			{if((theoricCol.size() > 0) && currentCol!=theoricCol.get(theoricCol.size()-1)) { setError(location,"The code is not indented.", yyline+1); }
                                 yybegin(COMMENT);}
<LINE>			{END_TYPE}		{if(theoricCol.size() > 0) theoricCol.remove(theoricCol.size()-1);
                                 yybegin(COMMENT);}
<LINE>			{END_INST}		{if((theoricCol.size() > 0) && theoricCol.get(theoricCol.size()-1) !=0 && currentCol!=theoricCol.get(theoricCol.size()-1)) { setError(location,"The code is not indented.", yyline+1); }
								 if(theoricCol.size() > 0) theoricCol.remove(theoricCol.size()-1);
                                 yybegin(COMMENT);}
<LINE>			{CONTINUE}		{if(numbers.contains(lastNum)) {numbers.remove(numbers.indexOf(lastNum)); theoricCol.remove(theoricCol.size()-1); }
								 if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1);
								 yybegin(COMMENT);}
<LINE>			#				{yybegin(COMMENT);}
<LINE>			{VAR}			{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
                                 yybegin(AVOID);}
<LINE>      	\n             	{currentCol=0; yybegin(NEW_LINE);}
<LINE>			{SPACE}			{currentCol+=yytext().length();}
<LINE>      	.              	{if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)) setError(location,"The code is not indented.", yyline+1); 
                                 yybegin(AVOID);}
                                 
/************************/
/* AVOID STATE    	    */
/************************/
<AVOID>			{STRING}		{}
<AVOID>			{TYPE}        	{endLine=true;theoricCol.add(0); location = yytext(); yybegin(NAMING);}
<AVOID>			{INST}			{endLine=true;
								 if(yytext().toLowerCase().contains("if")) { yybegin(IF_STATE);}
								 else { theoricCol.add(currentCol); yybegin(COMMENT);} }
<AVOID>			{ELSE}			{endLine=true;yybegin(COMMENT);}
<AVOID>			{END_TYPE}		{endLine=true;if(theoricCol.size() > 0) theoricCol.remove(theoricCol.size()-1); yybegin(COMMENT);}
<AVOID>			{END_INST}		{endLine=true;if(theoricCol.size() > 0) theoricCol.remove(theoricCol.size()-1); yybegin(COMMENT);}
<AVOID>			{VAR}			{endLine=true;}
<AVOID>			\&				{endLine=false;}
<AVOID>			#				{yybegin(COMMENT);}
<AVOID>			{SPACE}			{}
<AVOID>			\n				{if(endLine) { currentCol=0; yybegin(NEW_LINE); } }
<AVOID>			.				{endLine=true;}


/************************/
/* IF_STATE    		    */
/************************/
<IF_STATE>		{THEN}			{theoricCol.add(currentCol); yybegin(COMMENT);}
<IF_STATE>		&				{endLine=false;}
<IF_STATE>		#				{yybegin(COMMENT);}
<IF_STATE>		{SPACE}			{}
<IF_STATE>		\n				{if(endLine) { currentCol=0; yybegin(NEW_LINE);}}
<IF_STATE>		.				{endLine = true;}

/************************/
/* TYPE_STATE  		    */
/************************/
<TYPE_STATE>	\(				{yybegin(COMMENT);}
<TYPE_STATE>	\n				{theoricCol.add(currentCol); currentCol=0; yybegin(NEW_LINE);}
<TYPE_STATE>	{SPACE}			{}
<TYPE_STATE>	#				{yybegin(COMMENT);}
<TYPE_STATE>	.				{theoricCol.add(currentCol); currentCol=0; yybegin(COMMENT);}

/************************/
/* DO_STATE  		    */
/************************/
<DO_STATE>		{INT}			{numbers.add(Integer.parseInt(yytext())); yybegin(COMMENT);}
<DO_STATE>		{VAR}			{yybegin(COMMENT);}
<DO_STATE>		#				{yybegin(COMMENT);}
<DO_STATE>		\n				{yybegin(NEW_LINE);}
<DO_STATE>		.				{}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}