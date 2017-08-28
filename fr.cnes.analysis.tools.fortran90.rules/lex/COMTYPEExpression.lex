/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.TYPE.Expression rule. 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMTYPEExpression
%extends AbstractChecker
%public
%column
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, DECL_PARAMS, DECLARATION, AVOID, CONV_FUNC, REAL, DIMENSION, IF_STATE, AVOIDI, IO

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL	   | real	| COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
READ		 = "READ"
WRITE		 = "WRITE"
OPEN		 = "OPEN"
FORMAT 		 = "FORMAT"
KIND		 = "KIND"
CALL		 = "CALL"
DATA		 = "DATA"
PARAMETER	 = "PARAMETER"
DIMENSION	 = "DIMENSION" | "LEN"
IF			 = "IF"
CALL		 = "CALL" [\ ]+ {VAR}
IO			 = ({READ} 	  | {WRITE}		| {FORMAT}	| {CALL}	| {DATA}	|
			   {OPEN}	  | {PARAMETER}) [\ ]* (\()?
CONV		 = "INT"	  | "CEILING"	| "FLOOR"	| "NINT"	|
			   "DBLE"	  | "CMPLX"	 	| "IACHAR"	| "ACHAR"	|
			   "CHAR"	  | "DFLOAT"	| "FLOAT"	| "AIMAG"	|
			   "LEN"	  | "SIZE"
REAL		 = "REAL" [\ ]* \(
CONVERSION	 = {CONV} [\ ]* \(
OP_EXEP		 = \*		  | \*\*
EXP			 = \+		  | \-		   | "/"&&!"//"
LOGIC	     = \.AND\.	  | \.OR\.	   | \.NEQV\.		| \.XOR\.	|
			   \.EQV\.	  | \.NOT\.
RELAT		 = \.LT\.	  | \.LE\.	   | \.EQ\.			| \.NE\.	|
			   \.GT\.	  | \.GE\.     | \<             | \<\=      |
			   \=\=       | \/\=       | \>             | \>\=
OPERATOR     = {LOGIC}    | {RELAT} 
STRUCT		 = {VAR} (\([^\)]*\))? \%
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	Map<String, String> variables = new HashMap<String, String>();
	List<String> arrays = new LinkedList<String>();
	List<Integer> errors = new LinkedList<Integer>();
	String type;
	String expressionType = "empty";
	boolean exception = false;
	boolean expression = false;
	boolean error = false;
	boolean errorThrown = false;
	boolean dim = false;
	boolean isArray = false;
	boolean end = true;
	int par = 0;
	String conv = "";
	
	public COMTYPEExpression(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
	
	private void checkExpression(String var) throws JFlexException {
		String key = variables.get(var);
		if (key != null) {
			if(exception && expression) {
				if(!key.equals("integer") && !expressionType.equals(key) && !expressionType.equals("empty"))
					error = true;
				exception = false;
			}
			else  {
				if (expressionType.equals("empty")) 
					expressionType = key;
				else if (!expressionType.equals(key)) 
					error = true;
			}
			
			if (expression && error && !errorThrown && !errors.contains(yyline)) {
				errorThrown = true;
				setError(location,"Mixed type " + expressionType + " with " + key, yyline+1);
				errors.add(yyline);
			}
		}
		if (arrays.contains(var)) isArray = true;
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
<COMMENT>   	\n             	{error = false; errorThrown=false; yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext(); yybegin(COMMENT);}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>		{STRING}		{}
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{IO}			{yybegin(IO);}
<NEW_LINE>		{REAL}			{par++; yybegin(REAL);}
<NEW_LINE>		{DIMENSION}		{yybegin(DIMENSION);}
<NEW_LINE>		{DATA_TYPE}		{type=yytext().toUpperCase(); yybegin(DECL_PARAMS);}
<NEW_LINE>		{CONVERSION}	{par++; conv=yytext().toLowerCase(); yybegin(CONV_FUNC);}
<NEW_LINE>		{IF}			{yybegin(IF_STATE);}
<NEW_LINE>		{VAR}			{if(!isArray) { checkExpression(yytext()); }}
<NEW_LINE>		{VAR}[\ ]*\(	{String v = yytext().substring(0, yytext().length()-1).trim(); 
								 if(variables.get(v) != null) {par=1; checkExpression(v); yybegin(AVOID);} }
<NEW_LINE>		{OP_EXEP}		{exception=true; expression = true;}
<NEW_LINE>		{EXP}			{expression = true;}
<NEW_LINE>		{OPERATOR}		{expression = false; expressionType = "empty";}
<NEW_LINE>		\(				{par++;}
<NEW_LINE>		\)				{par--; if(isArray) isArray=false;}
<NEW_LINE>  	\n             	{expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false; par=0;}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{IO}			{yybegin(IO);}
<LINE>			{REAL}			{par++;yybegin(REAL);}
<LINE>			{DIMENSION}		{yybegin(DIMENSION);}
<LINE>			{DATA_TYPE}		{type=yytext().toUpperCase(); yybegin(DECL_PARAMS);}
<LINE>			{CONVERSION}	{par++; conv=yytext().toLowerCase(); yybegin(CONV_FUNC); end=true;}
<LINE>			{IF}			{yybegin(IF_STATE);}
<LINE>			{VAR}			{if(!isArray) { checkExpression(yytext()); } end=true;}
<LINE>			{VAR}[\ ]*\(	{String v = yytext().substring(0, yytext().length()-1).trim(); 
								 if(variables.get(v) != null) {par=1; checkExpression(v); yybegin(AVOID);} }
<LINE>			\({VAR}\)		{}
<LINE>			{OP_EXEP}		{exception=true; expression = true;}
<LINE>			{EXP}			{expression = true;}
<LINE>			{OPERATOR}		{expression = false; expressionType = "empty";}
<LINE>			\(				{par++;}
<LINE>			\)				{par--; if(isArray) isArray=false;}
<LINE>			\&				{end=false;}
<LINE>			{STRUCT}		{expressionType="empty"; expression=false; exception=false; error=false;}
<LINE>			\%	| \,		{expressionType="empty"; expression=false; exception=false; error=false;}
<LINE>      	\n             	{if(end) {expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false; par=0; yybegin(NEW_LINE);}}
<LINE>      	.              	{}


/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>	{STRING}		{}
<DECL_PARAMS>	\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>	{DIMENSION}		{dim = true; }
<DECL_PARAMS>  	\n             	{expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false; par=0; yybegin(NEW_LINE);}
<DECL_PARAMS>  	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}[\ ]* \(	{par++; String var = yytext(); var = var.substring(0, var.length()-1).trim();  
								 variables.put(var, type); arrays.add(yytext().substring(0, yytext().length()-1));
								 par=1; yybegin(AVOIDI);}
<DECLARATION>	{VAR}			{variables.put(yytext(), type);
								 if(dim) arrays.add(yytext());}
<DECLARATION>	\&				{end=false;}
<DECLARATION>  	\n             	{if (end ) {expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false; dim = false; par= 0; yybegin(NEW_LINE);}
								 end = true;}
<DECLARATION>  	.              	{}


/************************/
/* DIMENSION STATE  	*/
/************************/
<DIMENSION>		{STRING}		{}
<DIMENSION>		{VAR}			{arrays.add(yytext());}
<DIMENSION>  	\n             	{yybegin(NEW_LINE);}
<DIMENSION>  	.              	{}

/************************/
/* AVOID STATE 			*/
/************************/
<AVOID>			\(				{par++;}
<AVOID> 		\)            	{par--; if (par==0) yybegin(LINE);}
<AVOID>			[^]         	{}
<AVOIDI>		\(				{par++;}
<AVOIDI>		\)				{par--; if(par==0) yybegin(DECLARATION);}
<AVOIDI>		[^]			{}

/************************/
/* IO STATE    			*/
/************************/
<IO>			\&				{end=false;}
<IO>			\n				{if (end) yybegin(LINE);
								 end = true;}
<IO>  			.              	{}


/************************/
/* REAL STATE    		*/
/************************/
<REAL>			{STRING}		{}
<REAL>			{KIND}|\:\:|\,	{type="REAL"; yybegin(DECL_PARAMS);}
<REAL>  		{VAR}           {String key = variables.get(yytext());
								 if(key != null) { type="REAL"; yybegin(DECL_PARAMS);}
								 else {par=1; conv="real"; yybegin(CONV_FUNC);} }
<REAL>			\n				{expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false;par=0;yybegin(NEW_LINE);}
<REAL>  		.             	{}

/************************/
/* CONV_FUNC STATE    	*/
/************************/
<CONV_FUNC>		{STRING}		{}
<CONV_FUNC>		\(				{par++;}
<CONV_FUNC> 	\)            	{par--; if (par==0) {
									String et = "";
									if(conv.contains("real") || conv.contains("aimag")) et = "REAL";
									else if(conv.contains("dble")|| conv.contains("float")) et = "DOUBLE PRECISION";
									else if(conv.contains("cmplx")) et = "COMPLEX";
									else if(conv.contains("char") || conv.contains("len")) et = "CHARACTER";
									else et = "INTEGER";
									if(expression) {
										if(expressionType.equals("empty")) expressionType = et;
										else if(!et.equals(expressionType) && !errors.contains(yyline)) {
											setError(location,"Mixed type " + expressionType + " with " + et, yyline+1);
											errors.add(yyline);
										}
									} 
									yybegin(LINE);
								 }
								}
<CONV_FUNC>		\n				{expressionType="empty"; expression = false; exception = false; error = false; errorThrown=false; isArray = false; par=0; yybegin(NEW_LINE);}
<CONV_FUNC>  	.              	{}


/************************/
/* IF_STATE STATE   	*/
/************************/
<IF_STATE>		{STRING}		{}
<IF_STATE>		{VAR}			{if(!isArray) { checkExpression(yytext()); } end=true;}
<IF_STATE>		{OP_EXEP}		{exception=true; expression = true;}
<IF_STATE>		{EXP}			{expression = true;}
<IF_STATE>		{OPERATOR}		{expression = false; expressionType = "empty";}
<IF_STATE>		\(				{par++;}
<IF_STATE>		\)				{par--; if(isArray) isArray=false;
								 if(par==0){expressionType="empty"; expression = false; exception = false; error = false; isArray = false; yybegin(LINE);}}
<IF_STATE>     	\n             	{}
<IF_STATE>     	.              	{}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }