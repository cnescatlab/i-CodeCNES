/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.FileExistence rule.*/
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

%class COMFLOWFileExistence
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, IO, INQ, INQ_EXIST, DECL_PARAMS, DECLARATION, ALLOC

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
OPEN		 = OPEN		  | open
READ		 = READ		  | read
WRITE		 = WRITE	  | write
IO			 = {READ}     | {WRITE}		| {OPEN}
UNIT		 = "UNIT "	  | "UNIT="
FILE		 = "FILE "	  | "FILE="
TRIM		 = "TRIM "	  | "TRIM("
EXIST 		 = EXIST 	  | exist
INQUIRE		 = INQUIRE	  | inquire
ALLOC		 = ALLOCATE   | allocate
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

																
%{
	String location = "MAIN PROGRAM";
	List<String> files = new LinkedList<String>();
	List<String> chars = new LinkedList<String>();
	boolean endLine = true;
	
	public COMFLOWFileExistence(){
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

				{FREE_COMMENT}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); files.clear(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<NEW_LINE>		{INQUIRE}		{yybegin(INQ);}
<NEW_LINE>		{ALLOC}			{yybegin(ALLOC);}
<NEW_LINE>		{IO}			{yybegin(IO);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{location=yytext(); yybegin(NAMING);}
<LINE>			{STRING}		{}
<LINE>			{DATA_TYPE}		{yybegin(DECL_PARAMS);}
<LINE>			{INQUIRE}		{yybegin(INQ);}
<LINE>			{ALLOC}			{yybegin(ALLOC);}
<LINE>			{IO}			{yybegin(IO);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS> 	\n             	{yybegin(NEW_LINE);}
<DECL_PARAMS>	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{VAR}			{chars.add(yytext());}
<DECLARATION> 	\n             	{yybegin(NEW_LINE);}
<DECLARATION>	.              	{}


/************************/
/* INQ STATE    	    */
/************************/
<INQ>			{UNIT}|{FILE}	{}
<INQ>			{VAR}			{files.add(yytext()); yybegin(INQ_EXIST);}
<INQ>     	 	\n             	{}
<INQ>      		.              	{}

/************************/
/* INQ_EXIST STATE    	*/
/************************/
<INQ_EXIST>		{EXIST}			{yybegin(COMMENT);}
<INQ_EXIST>		\&				{endLine=false;}
<INQ_EXIST>  	\n             	{if(endLine) {
									files.remove(files.size()-1);
									yybegin(NEW_LINE);
								 } endLine = true; }
<INQ_EXIST> 	.              	{}


/************************/
/* ALLOC STATE    	    */
/************************/
<ALLOC>			{VAR}			{files.add(yytext()); yybegin(COMMENT);}
<ALLOC>      	\n             	{}
<ALLOC>     	.              	{}

/************************/
/* IO STATE    	 	    */
/************************/
<IO>			{UNIT}|{TRIM}	{}
<IO>			{VAR}			{if(!files.contains(yytext()) && !chars.contains(yytext())) {
									setError(location,"The existences of the file " + yytext() + " must be checked with the instruction INQUIRE before being opened or created. ", yyline+1);
								 } yybegin(COMMENT);}
<IO>			\*				{yybegin(COMMENT);}
<IO>			\&				{endLine=false;}
<IO>	  		\n             	{if(endLine)yybegin(NEW_LINE);
								 endLine = true; }
<IO>      		.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}