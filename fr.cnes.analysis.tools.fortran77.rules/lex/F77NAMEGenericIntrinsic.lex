/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/************************************************************************************/
/* This file is used to generate a rule checker for F77.NAME.GenericIntrinsic rule. */
/* For further information on this, we advise you to refer to RNC manuals.		    */
/* As many comments have been done on the ExampleRule.lex file, this file    		*/
/* will restrain its comments on modifications.								 		*/
/*																			 		*/
/************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class F77NAMEGenericIntrinsic
%extends AbstractChecker
%public
%line
%column
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, RIGHT_SIDE, DECLARATION

COMMENT_WORD = \!         | c          | C		| \*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL	   | real	| COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]

																
%{
	String location = "MAIN PROGRAM";
	
	List<String> intrinseques =  new LinkedList<String>();
	List<String> intrinsequesGeneriques =  new LinkedList<String>();
	List<String> variables = new LinkedList<String>(); 
	/** name of the file parsed */
	private String parsedFileName;
	
	public F77NAMEGenericIntrinsic() {
		List<String> intr = Arrays.asList("IFIX","IDINT","FLOAT","SNGL","ICHAR","CHAR","DINT","DNINT","IDNINT","IABS","DABS","CABS","AMOD","DMOD","ISIGN","DSIGN","IDIM","DDIM","DPROD","MAX0","AMAX1","DMAX1","AMAX0","MAX1","MIN0","AMIN1","DMIN1","AMIN0","MIN1","DSQRT","CSQRT","DEXP","CEXP","ALOG","DLOG","CLOG","ALOG10","DLOG10","DSIN","CSIN","DCOS","CCOS","DTAN","DASIN","DACOS","DATAN","DATAN2","DSINH","DCOSH","DTANH");
		intrinseques.addAll(intr);
		List<String> intrGene = Arrays.asList("INT","REAL","DBLE","CMPLX","AINT","ANINT","NINT","ABS","MOD","SIGN","DIM","CMPLX","MAX","MIN","SQRT","EXP","LOG","LOG10","SIN","COS","TAN","ASIN","ACOS","ATAN","ATAN2","SINH","COSH","TANH");
		intrinsequesGeneriques.addAll(intrGene);
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}

	private void checkFunctionName(String text) throws JFlexException {
		if (intrinseques.contains(text) && !intrinsequesGeneriques.contains(text) && !variables.contains(text)) {
			this.setError(location,"It should be used the generic name of the intrinsic function instead of " + text, yyline + 1);
		}
	}
	
%}

%eofval{
    return getCheckResults();
%eofval}

%%          

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{DATA_TYPE}		{yybegin(DECLARATION);}
<NEW_LINE>		\=				{yybegin(RIGHT_SIDE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{yybegin(LINE);}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{DATA_TYPE}		{yybegin(DECLARATION);}
<LINE>			\=				{yybegin(RIGHT_SIDE);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<RIGHT_SIDE>	{VAR}			{checkFunctionName(yytext()); yybegin(LINE);}
<RIGHT_SIDE>	\n				{yybegin(NEW_LINE);}
<RIGHT_SIDE>	.				{}

<DECLARATION>	{VAR}			{variables.add(yytext());}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}
<DECLARATION>  	\n             	{yybegin(NEW_LINE);}
<DECLARATION>  	.              	{}

				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);	
								}