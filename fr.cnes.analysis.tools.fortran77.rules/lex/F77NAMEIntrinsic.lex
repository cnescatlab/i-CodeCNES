/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F77.NAME.Intrinsic rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;


%%

%class F77NAMEIntrinsic
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE, FUNCTION

COMMENT_WORD = \!         | c          | C
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {PROC}	   | {SUB} | {PROG} | {MOD}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

																
%{
	String location = "MAIN PROGRAM";
	
	List<String> intrinseques =  new LinkedList<String>(); 
	/** name of the file parsed */
	private String parsedFileName;
	
	public F77NAMEIntrinsic() {
		//readFile();
		List<String> intr = Arrays.asList("INT","IFIX","IDINT","REAL","FLOAT","SNGL","DOUBLE","COMPLEX","ICHAR","CHAR","AINT","DINT","ANINT","DNINT","NINT","IDNINT","IABS","ABS","DABS","CABS","MOD","AMOD","DMOD","ISIGN","SIGN","DSIGN","IDIM","DIM","DDIM","DPROD","MAX0","AMAX1","DMAX1","AMAX0","MAX1","MIN0","AMIN1","DMIN1","AMIN0","MIN1","LEN","INDEX","AIMAG","CONJG","SQRT","DSQRT","CSQRT","EXP","DEXP","CEXP","ALOG","DLOG","CLOG","ALOG10","DLOG10","SIN","DSIN","CSIN","COS","DCOS","CCOS","TAN","DTAN","ASIN","DASIN","ACOS","DACOS","ATAN","DATAN","ATAN2","DATAN2","SINH","DSINH","COSH","DCOSH","TANH","DTANH","LGE","LGT","LLE","LLT");
		intrinseques.addAll(intr);
    }

	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}

	private void checkFunctionName(String text) throws JFlexException {
		if (intrinseques.contains(text)) {
			this.setError(location,"It is not allowed to use the name of an intrinsic function.", yyline + 1);
		}
	}
	
	private void readFile() throws JFlexException {
		try{
			// Open the file that is the first 
			// command line parameter
			FileInputStream fstream = new FileInputStream("files/intrinsiques.txt");
			// Get the object of DataInputStream
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String strLine;
			//Read File Line By Line
			while ((strLine = br.readLine()) != null)   {
					intrinseques.add(strLine);
		  	}
		  	//Close the input stream
		  	in.close();
	    }
		catch (Exception e){
	    	throw new JFlexException(e);
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
<YYINITIAL>		{FUNC}        	{location = yytext();
								 yybegin(FUNCTION);}
<YYINITIAL>  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{FUNC}         	{location = yytext();
								 yybegin(FUNCTION);}
<NEW_LINE>  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{yybegin(LINE);}
<LINE>      	{FUNC}         	{location = yytext();
								 yybegin(FUNCTION);}
<LINE>		  	{TYPE}         	{location = yytext();
								 yybegin(NAMING);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

<FUNCTION>		{VAR}			{checkFunctionName(yytext());
								 yybegin(LINE);}
<FUNCTION>		\n				{yybegin(NEW_LINE);}
<FUNCTION>		.				{}

				[^]           {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
								}