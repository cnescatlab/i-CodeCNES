/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.DATA.Float rule.	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DATAFloat
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
/* and some new states can be added.											*/
%state COMMENT, NAMING, NEW_LINE, LINE, DECLARATION, SORTIE, AVOID_DECL, DECL_PARAMS

/* These are the words which are involved in automaton's transition. 	*/
/* COMMENT_WORD determines when a comment start.						*/
/* FUNC, PROC, SUB, PROG and MOD are used to differ program's part.		*/
/* VAR is used to recognize a variable or function name.				*/
/* STRING is used to identify a string variable.						*/

COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
FLOAT		 = "REAL"
WRITE		 = WRITE	  | write
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.												*/
%{
	/** Variable used to determine violations' location. **/
    String location = "MAIN PROGRAM";
    final List<String> floatList = new LinkedList<String>();
    /** Boolean used to determine if star is used in a write. **/
    boolean star = false;
    /** Boolean used to deal with line continuation. **/
    boolean end = true;

    /**
     * Empty constructor.
     */
    public F90DATAFloat() {
    }

    /*
     * (non-Javadoc)
     * @see
     * fr.cnes.analysis.tools.analyzer.datas.AbstractRule#setInputFile(org.
     * eclipse.core.runtime.IPath)
     */
    @Override
    public final void setInputFile(final File file)
            throws FileNotFoundException {
        super.setInputFile(file);
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
    }
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{ 
 return getViolations(); 
%eofval}


%%          


				{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{FLOAT}			{yybegin(DECLARATION);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{FLOAT}			{yybegin(DECLARATION);}
<NEW_LINE>		{WRITE}			{yybegin(SORTIE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{FLOAT}			{yybegin(DECL_PARAMS);}
<LINE>			{WRITE}			{yybegin(SORTIE);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>	\n				{yybegin(NEW_LINE);}
<DECL_PARAMS>	.				{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{VAR}			{end = true; floatList.add(yytext());}
<DECLARATION>	&				{end = false;}
<DECLARATION>	\=				{yybegin(AVOID_DECL);}
<DECLARATION>	\n				{if(end) yybegin(NEW_LINE);}
<DECLARATION>	.				{}


/************************/
/* AVOID_DECL STATE   	*/
/************************/
<AVOID_DECL>		\,			{end=true; yybegin(DECLARATION);}
<AVOID_DECL>		&			{end=false;}
<AVOID_DECL>		\n			{if(end) yybegin(NEW_LINE);}
<AVOID_DECL>		.			{}


/************************/
/* SORTIE STATE   	    */
/************************/
<SORTIE>		\*				{star=true;}
<SORTIE>		{VAR}			{if(star && floatList.contains(yytext())) setError(location,"It is not allowed to use the format * for reals like " + yytext(), yyline+1);}
<SORTIE>		\n				{star=false; yybegin(NEW_LINE);}
<SORTIE>		.				{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
