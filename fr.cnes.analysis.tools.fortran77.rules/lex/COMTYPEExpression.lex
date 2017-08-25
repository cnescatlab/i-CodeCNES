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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMTYPEExpression
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, DECL_PARAMS, DECLARATION, IO, CONV_FUNC, REAL, AVOID, AVOIDI

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
PRINT		 = "PRINT"
KIND		 = "KIND"
CALL		 = "CALL"
DATA		 = "DATA"
PARAMETER	 = "PARAMETER"
IO			 = ({READ} 	  | {WRITE}		| {FORMAT}	| {CALL}	| {DATA}	|
			   {OPEN}	  | {PARAMETER} | {PRINT}) [\ ]* (\()?
IF			 = "IF"[\ ]*\(
CONV		 = "INT"	  | "CEILING"	| "FLOOR"	| "NINT"	|
			   "DBLE"	  | "CMPLX"	 	| "IACHAR"	| "ACHAR"	|
			   "CHAR"	  | "DFLOAT"	| "FLOAT"	| "AIMAG"	|
			   "LEN"	  | "SIZE"		| "DIMAG"	| "DCMPLX"
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
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]	| \*
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMTYPEExpression.class.getName());

	String location = "MAIN PROGRAM";
    String parsedFileName;
	
	Map<String, String> variables = new HashMap<String, String>();
	List<String> arrays = new LinkedList<String>();
	List<Integer> errors = new LinkedList<Integer>();
	String type;
	String expressionType = "empty";
	boolean exception = false;
	boolean expression = false;
	boolean error = false;
	boolean dim = false;
	boolean isArray = false;
	int par = 0;
	String conv = "";
	
	public COMTYPEExpression(){
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	private void checkExpression (String var) throws JFlexException {
        LOGGER.finest("begin method checkExpression");
		String key = variables.get(var);
		if (key != null) {
			if(exception && expression) {
				if(!key.equals("integer") && !expressionType.equals(key) && !expressionType.equals("empty"))
					error = true;
				exception = false;
			}
			else  {
				if (expressionType.equals("empty") ) 
					expressionType = key;
				else if (!expressionType.equals(key) ) 
					error = true;
			}
			
			if (expression && error && !errors.contains(yyline)) {
				LOGGER.fine("Setting error line "+(yyline+1)+" because of mixed type in expression \""+ expressionType +"\" with \""+key+"\" .");
				setError(location,"Mixed type " + expressionType + " with " + key , yyline+1);
				errors.add(yyline);
			}
		}
		if (arrays.contains(var)) isArray = true;
        LOGGER.finest("end method checkExpression");
	}

	
%}

%eofval{
    return getCheckResults();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
				                    yybegin(COMMENT);}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{error = false; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NAMING>    	\n             	{par=0; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>		{STRING}		{}
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
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{TYPE}        	{location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<NEW_LINE>		{IO}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IO (Transition : IO \""+yytext()+"\" )");
                                    yybegin(IO);}
<NEW_LINE>		{REAL}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> REAL (Transition : REAL \""+yytext()+"\" )");
                                    yybegin(REAL);}
<NEW_LINE>		{DATA_TYPE}		{type=yytext().toUpperCase(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECLARATION (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECLARATION);}
<NEW_LINE>		{CONVERSION}	{par=1; conv=yytext().toLowerCase();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> CONV_FUNC (Transition : CONVERSION \""+yytext()+"\" )");
                                    yybegin(CONV_FUNC);}
<NEW_LINE>		{IF}			{par=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : IF \""+yytext()+"\" )");
                                    yybegin(AVOID);}
<NEW_LINE>		{VAR}			{if(!isArray || par==0) { checkExpression(yytext()); }}
<NEW_LINE>		{VAR}[\ ]*\(	{
                                    String v = yytext().substring(0, yytext().length()-1).trim();
                                    if(variables.get(v) != null) {
                                        par=1;
                                        checkExpression(v);
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : VAR \""+yytext()+"\" )");
                                        yybegin(AVOID);
                                    }
                                }
<NEW_LINE>		{OP_EXEP}		{exception=true; expression = true;}
<NEW_LINE>		{EXP}			{expression = true;}
<NEW_LINE>		{OPERATOR}		{expression = false; expressionType = "empty";}
<NEW_LINE>		\(				{par++;}
<NEW_LINE>		\)				{par--; if(isArray) isArray=false;}
<NEW_LINE>  	\n             	{par=0; expressionType="empty"; expression = false; error = false; isArray = false;}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<LINE>			{IO}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> IO (Transition : IO \""+yytext()+"\" )");
                                    yybegin(IO);}
<LINE>			{REAL}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> REAL (Transition : REAL \""+yytext()+"\" )");
                                    yybegin(REAL);}
<LINE>			{DATA_TYPE}		{type=yytext().toUpperCase();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DECLARATION (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(DECLARATION);}
<LINE>			{CONVERSION}	{par=1; conv=yytext().toLowerCase();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> CONV_FUNC (Transition : CONVERSION \""+yytext()+"\" )");
                                    yybegin(CONV_FUNC);}
<LINE>			{IF}			{par=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : IF \""+yytext()+"\" )");
                                    yybegin(AVOID);}
<LINE>			{VAR}			{if(!isArray || par==0) { checkExpression(yytext()); }}
<LINE>			{VAR}[\ ]*\(	{String v = yytext().substring(0, yytext().length()-1).trim(); 
								 if(variables.get(v) != null) {
								    par=1;
								    checkExpression(v);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(AVOID);
                                 }
                                }
<LINE>			{OP_EXEP}		{exception=true; expression = true;}
<LINE>			{EXP}			{expression = true;}
<LINE>			{OPERATOR}		{expression = false; expressionType = "empty";}
<LINE>			\(				{par++;}
<LINE>			\)				{par--; if(isArray) isArray=false;}
<LINE>			\n[\ ]{1,5}{SIMBOL}	{}
<LINE>      	\n             	{par=0; expressionType="empty"; expression = false; error = false; isArray = false; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{STRING}		{}
<DECLARATION>	{VAR}[\ ]* \(	{
                                    String var = yytext(); var = var.substring(0, var.length()-1).trim();  
								    variables.put(var, type); arrays.add(yytext().substring(0, yytext().length()-1));
								    par=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> AVOIDI (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(AVOIDI);}
<DECLARATION>	{VAR}			{variables.put(yytext(), type);
								 if(dim) arrays.add(yytext());}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}			
<DECLARATION>  	\n             	{par=0;  dim = false; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<DECLARATION>  	.              	{}

/************************/
/* IO STATE    			*/
/************************/
<IO>			\n[\ ]{1,5}{SIMBOL}	{}
<IO>			\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IO -> LINE (Transition : \\n )");
                                    yybegin(LINE);}
<IO>  			.              	{}


/************************/
/* REAL STATE    		*/
/************************/
<REAL>			{STRING}		{}
<REAL>			{KIND}|\:\:|\,	{type="REAL"; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - REAL -> DECL_PARAMS (Transition : KIND \""+yytext()+"\" )");
                                    yybegin(DECL_PARAMS);}
<REAL>  		{VAR}           {par=1; conv="real"; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - REAL -> CONV_FUNC (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(CONV_FUNC);}
<REAL>			\n				{par=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - REAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<REAL>  		.             	{}

/************************/
/* CONV_FUNC STATE    	*/
/************************/
<CONV_FUNC>		{STRING}		{}
<CONV_FUNC>		\(				{par++;}
<CONV_FUNC> 	\)            	{par--; if (par==0) {
									String et = "";
									if(conv.contains("real") || conv.contains("aimag")) et = "REAL";
									else if(conv.contains("dble")|| conv.contains("float") || conv.contains("dimag") ||
											conv.contains("dcmplx")) et = "DOUBLE PRECISION";
									else if(conv.contains("cmplx")) et = "COMPLEX";
									else if(conv.contains("char") || conv.contains("len")) et = "CHARACTER";
									else et = "INTEGER";
									if(expression) {
										if(expressionType.equals("empty")) expressionType = et;
										else if(!et.equals(expressionType)  && !errors.contains(yyline))  {
											LOGGER.fine("Setting error line "+(yyline+1)+" because of mixed type \""+ expressionType +"\" with \""+et+"\" .");
											setError(location,"Mixed type " + expressionType + " with " + et, yyline+1);
											errors.add(yyline);	
										}
									} 
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - CONV_FUNC -> LINE (Transition : [)] )");
									yybegin(LINE);
								 }
								}
<CONV_FUNC>		\n				{par=0; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - CONV_FUNC -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<CONV_FUNC>  	.              	{}

/************************/
/* AVOID STATE	        */
/************************/
<AVOID>			{STRING}		{}
<AVOID>			\(				{par++;}
<AVOID>			\)				{par--; 
                                    if(par==0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> LINE (Transition : [)] )");
                                        yybegin(LINE);
                                    }
                                }
<AVOID>			[^]			{}
<AVOIDI>		\(				{par++;}
<AVOIDI>		\)				{par--; if(par==0){
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOIDI -> DECLARATION (Transition : [)] )");
                                    yybegin(DECLARATION);
                                }
                                }
<AVOIDI>		[^]			{}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                               }
