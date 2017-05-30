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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

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
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, IO, INQ, INQ_EXIST, DECL_PARAMS, DECLARATION

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
TRIM		 = "TRIM "    | "TRIM("
EXIST 		 = EXIST 	  | exist
INQUIRE		 = INQUIRE	  | inquire
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMFLOWFileExistence.class.getName());

	String location = "MAIN PROGRAM";
	
	List<String> files = new LinkedList<String>();
	List<String> chars = new LinkedList<String>();
	boolean endLine = true;
    String parsedFileName;
	
	public COMFLOWFileExistence(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}

	
%}

%eofval{
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
<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); files.clear();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NAMING>    	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");   
                                    yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECL_PARAMS (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECL_PARAMS);}
<NEW_LINE>		{INQUIRE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> INQ (Transition : INQUIRE \""+yytext()+"\" )");
                                    yybegin(INQ);}
<NEW_LINE>		{IO}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IO (Transition : IO \""+yytext()+"\" )");
                                    yybegin(IO);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{TYPE}        	{
                                    location=yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<LINE>			{STRING}		{}
<LINE>			{DATA_TYPE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DECL_PARAMS (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECL_PARAMS);}
<LINE>			{INQUIRE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> INQ (Transition : INQUIRE \""+yytext()+"\" )");
                                    yybegin(INQ);}
<LINE>			{IO}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> IO (Transition : IO \""+yytext()+"\" )");
                                    yybegin(IO);}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	\:\:			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECL_PARAMS -> DECLARATION (Transition : :: )");
                                    yybegin(DECLARATION);}
<DECL_PARAMS> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECL_PARAMS -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<DECL_PARAMS>	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{VAR}			{chars.add(yytext());}
<DECLARATION> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<DECLARATION>	.              	{}


/************************/
/* INQ STATE    	    */
/************************/
<INQ>			{UNIT}|{FILE}	{}
<INQ>			{VAR}			{files.add(yytext());
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INQ -> INQ_EXIST (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(INQ_EXIST);}
<INQ>     	 	\n             	{}
<INQ>      		.              	{}

/************************/
/* INQ_EXIST STATE    	*/
/************************/
<INQ_EXIST>		{EXIST}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INQ_EXIST -> COMMENT (Transition : EXIST \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<INQ_EXIST>		\&				{endLine=false;}
<INQ_EXIST>  	\n             	{if(endLine) {
									files.remove(files.size()-1);
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INQ_EXIST -> NEW_LINE (Transition : \\n )");
									yybegin(NEW_LINE);
								 } endLine = true; }
<INQ_EXIST> 	.              	{}


/************************/
/* IO STATE    	 	    */
/************************/
<IO>			{UNIT}|{TRIM}	{}
<IO>			{VAR}			{if(!files.contains(yytext()) && !chars.contains(yytext())) {
									LOGGER.fine("Setting error line "+(yyline+1)+" because of the variable \""+yytext()+"\"");
									setError(location,"The existences of the file " + yytext() + " must be checked with the instruction INQUIRE before being opened or created. ", yyline+1);
								 }
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IO -> COMMENT (Transition : VAR \""+yytext()+"\" )");
								 yybegin(COMMENT);
								 
								}
<IO>			\*				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IO -> COMMENT (Transition : * )");
                                    yybegin(COMMENT);}
<IO>			\&				{endLine=false;}
<IO>	  		\n             	{if(endLine){
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IO -> COMMENT (Transition : \\n )");
                                    yybegin(NEW_LINE);
								 }
								 endLine = true; }
<IO>      		.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]           {
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+(yyline+1)+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                                }