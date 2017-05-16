/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.NAME.GenericIntrisic */
/* rule. 																	 */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90NAMEGenericIntrinsic
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT, NAMING, NEW_LINE, LINE

COMMENT_WORD = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
VAR_PAR	     = {VAR} [\ ]* \(
STRING		 = \'[^\']*\' | \"[^\"]*\"

%{
	String location = "MAIN PROGRAM"; 
  List<Violation> list = new LinkedList<Violation>();
	List<String> intrinseques =  new LinkedList<String>();
	List<String> intrinsequesGeneriques =  new LinkedList<String>();
	
	public F90NAMEGenericIntrinsic() {
		List<String> intr = Arrays.asList("INT","IFIX","IDINT","REAL","FLOAT","SNGL","ICHAR","CHAR","AINT","DINT","ANINT","DNINT","NINT","IDNINT","IABS","ABS","DABS","CABS","MOD","AMOD","DMOD","ISIGN","SIGN","DSIGN","IDIM","DIM","DDIM","DPROD","MAX0","AMAX1","DMAX1","AMAX0","MAX1","MIN0","AMIN1","DMIN1","AMIN0","MIN1","AIMAG","CONJG","SQRT","DSQRT","CSQRT","EXP","DEXP","CEXP","ALOG","DLOG","CLOG","ALOG10","DLOG10","SIN","DSIN","CSIN","COS","DCOS","CCOS","TAN","DTAN","ASIN","DASIN","ACOS","DACOS","ATAN","DATAN","ATAN2","DATAN2","SINH","DSINH","COSH","DCOSH","TANH","DTANH");
		intrinseques.addAll(intr);
		List<String> intrGene = Arrays.asList("INT","REAL","AINT","ANINT","NINT","ABS","MOD","SIGN","DIM","MAX","MIN","SQRT","EXP","LOG","LOG10","SIN","COS","TAN","ASIN","ACOS","ATAN","ATAN2","SINH","COSH","TANH");
		intrinsequesGeneriques.addAll(intrGene);
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void checkName(String variable) throws JFlexException{
		variable = variable.substring(0, variable.length()-1).trim();
		if (intrinseques.contains(variable.toUpperCase()) && !intrinsequesGeneriques.contains(variable.toUpperCase())) {
			setError(location,"Use the generic name of the intrinsic functions instead of " + variable, yyline+1);
		}
	}
	
%}

%eofval{ 
  
 return getViolations(); 
%eofval}

/* Transition word is entry (or ENTRY). This word must not be found. */
/* Whenever it's found, an error is returned.						 */

%%          

				{COMMENT_WORD}	{yybegin(COMMENT);}
				  
/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE      */
/************************/
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{VAR_PAR}		{checkName(yytext());}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE           */
/************************/
<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{VAR_PAR}		{checkName(yytext());}	
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
