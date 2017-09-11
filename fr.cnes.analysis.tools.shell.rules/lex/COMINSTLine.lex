/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Line rule.			  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

import java.util.logging.Logger;


%%

%class COMINSTLine
%extends AbstractChecker
%public
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state IGNORE, NAMING, IF, DO

COMMENT_WORD 			= [\#]
FUNCTION  			  	= "function"
FUNCT					= {VAR}{SPACE}*\(\)
SPACE					= [\ \r\t\f]
VALUE					= [0-9][0-9]*([\.][0-9][0-9]*)?
VAR					  	= ([a-zA-Z][a-zA-Z0-9\_\-]*)|([$]([\-\@\?\#\!\_\*]|([a-zA-Z0-9]*)|[\{][a-zA-Z0-9]*[\}]))
STRING				 	= \'[^\']*\' | \"[^\"]*\"
NEW_INSTRUCTION 		= [\;]{SPACE}*{VAR}+
OPERATOR 	 			= [\!\|\=\+\-\*\/\%]
BRACKET					= [\(\[\]\)]
IF						= "if"
DO_CONSTRUCT			= "while" | "for" | "until" | "select"
DO						= "do"
THEN					= "then"
CONDITIONAL_STRUCT		= [\[][\[]({VAR}|{SPACE}|{VALUE}|{OPERATOR}|{BRACKET})*[\]][\]] | [\(][\(]({VAR}|{SPACE}|{VALUE}|{OPERATOR}|{BRACKET})*[\)][\)]


																
%{
    private static final Logger LOGGER = Logger.getLogger(COMINSTLine.class.getName());
	String location = "MAIN PROGRAM";
    private String parsedFileName;
    
	

    public COMINSTLine() {
    	
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.fine("begin method setInputFile");
        
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.fine("end method setInputFile");
	}
			
%}

%eofval{
	return getCheckResults();
%eofval}


%%          



/************************/



/************************/
/* IGNORE STATE	    */
/************************/
<IGNORE>   	
		{
				\n             		{
				                    	LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IGNORE -> YYINITIAL (Transition : \\n )");
										yybegin(YYINITIAL);
									}  
			   	{NEW_INSTRUCTION}	{}
			   	.              		{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{	
									location = yytext();
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : VAR \""+yytext()+"\" )");
									yybegin(YYINITIAL);}
				\n             	{
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n )");
									yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* IF STATE	    */
/************************/
<IF>
		{
			\n | {THEN} 	{yybegin(YYINITIAL);}
			.				{}
		}
/************************/
/* DO STATE	    */
/************************/
<DO>
		{
			\n | {DO} 		{yybegin(YYINITIAL);}
			.				{}
		}
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 			{
			  								LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IGNORE (Transition : COMMENT_WORD \""+yytext()+"\" )");
			  								yybegin(IGNORE);
		  								}
				{CONDITIONAL_STRUCT}	{}
				{DO_CONSTRUCT}			{	
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> DO (Transition : DO \""+yytext()+"\" )");
											yybegin(DO);}
				{IF}					{	
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IF (Transition : IF \""+yytext()+"\" )");
											yybegin(IF);}
				{FUNCTION}     			{
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : FUNCTION \""+yytext()+"\" )");
											yybegin(NAMING);}
				{FUNCT}					{location = yytext().substring(0,yytext().length()-2).trim(); }
			    {STRING}				{}
			    {NEW_INSTRUCTION}	 	{
			    							LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - setError on \""+yytext()+"\" )");
			    							setError(location,"More than one instruction by line is not allowed.", yyline+1);
			    							LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IGNORE (NEW_INSTRUCTION : \""+yytext()+"\")");
			    							yybegin(IGNORE);
			    						}
	      		[^]            			{}
		}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}