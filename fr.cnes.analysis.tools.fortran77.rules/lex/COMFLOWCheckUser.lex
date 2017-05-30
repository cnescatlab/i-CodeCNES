/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.CheckUser rule. 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMFLOWCheckUser
%extends AbstractRule
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, CHECK_USER, NAMING_PROGRAM

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
TYPE		 = "function"  | "procedure" | "subroutine" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

GETUID		 = "GETUID"{SPACE}*\({SPACE}*\)
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMFLOWCheckUser.class.getName());

	String location = "PROGRAM";
	boolean getuid = false;
	boolean prg = false;
	int line = 0;
	String parsedFileName;
	
	public COMFLOWCheckUser(){
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
	if(!getuid && prg){
	   LOGGER.fine("Setting error line "+(line+1)+" cause the user identity is not verified in the main program.");
	   setError(location,"The user identity is not verified in the main program.", line+1);
    }
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
<COMMENT>   	
		{
			\n             	{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                    			yybegin(NEW_LINE);
                    			}  
			.              	{}
		}

/************************/
/* NAMING STATE	        */
/************************/
<NAMING>
		{
			{VAR}			{location = location + " " + yytext(); 
							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
							 yybegin(COMMENT);}
			\n             	{
			                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
			                 yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* NAMING_PROGRAM STATE	*/
/************************/
<NAMING_PROGRAM>
		{
			{VAR}			{location = "PROGRAM " + yytext(); 
							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING_PROGRAM -> CHECK_USER (Transition : VAR \""+yytext()+"\" )");
							 yybegin(CHECK_USER);}
			\n             	{
			                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING_PROGRAM -> CHECK_USER (Transition : \\n )");
			                 yybegin(CHECK_USER);}
			.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	
		{
			{COMMENT_WORD}	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			                     yybegin(COMMENT);}
			"program"		{line=yyline; prg=true;
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING_PROGRAM (Transition : \"program\" )");
                                yybegin(NAMING_PROGRAM);}
			{TYPE}        	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
			                     yybegin(NAMING);}
			\n             	{
					             LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");        
			                     yybegin(NEW_LINE);}
			.              	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");   
			                     yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{COMMENT_WORD}	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			                     yybegin(COMMENT);}
			"program"		{line=yyline; prg=true;
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING_PROGRAM (Transition : \"program\" )");
                                yybegin(NAMING_PROGRAM);}
			{STRING}		{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
			                     yybegin(LINE);}
			{FALSE}			{}
			{TYPE}         	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
			                     yybegin(NAMING);}
			\n             	{}
			.              	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");  
			                     yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		  	
		{
			{COMMENT_WORD}	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
			                     yybegin(COMMENT);}
			"program"		{line=yyline; prg=true; 
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING_PROGRAM (Transition : \"program\" )");
			                     yybegin(NAMING_PROGRAM);}
			{STRING}		{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
			                     yybegin(LINE);}
			{TYPE}         	{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
			                     yybegin(NAMING);}
			\n             	{
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )"); 
                                yybegin(NEW_LINE);}
			.              	{}
		}



/************************/
/* CHECK_USER STATE     */
/************************/
<CHECK_USER>			
		{
			{GETUID}				{getuid=true;}
			\n             			{}
			.              			{}
		}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+(yyline+1)+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                                }