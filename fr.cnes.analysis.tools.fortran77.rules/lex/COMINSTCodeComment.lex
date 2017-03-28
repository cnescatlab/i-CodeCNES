/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.CodeComment rule.	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/* 		                                									    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTCodeComment
%extends AbstractRule
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, PREHEADER, HEADER, HEADER_2, AVOID, PARAMS

COMMENT_WORD = \!		  | c	       | C    | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTER		 = INTERFACE  | interface 
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTER}

CLE_F77		 = ASSIGN | BACKSPACE | BLOCK[\ ]+DATA | CALL | 
			   CLOSE[\ ]*\( | COMMON | CONTINUE | DATA | DIMENSION | 
			   DO | ELSE | END | ENDFILE | ENDIF | ENTRY | EQUIVALENCE | 
			   EXTERNAL | FORMAT | FUNCTION | GOTO | IF | IMPLICIT | 
			   INQUIRE | INTRINSIC | OPEN[\ ]*\( | PARAMETER | PAUSE | 
			   PRINT[\ ]*\( | PROGRAM | READ[\ ]*\( | RETURN | REWIND | 
			   REWRITE | SAVE[\ ]*\( | STOP | SUBROUTINE | WRITE[\ ]*\( | 
			   assign | backspace | block[\ ]+data | call | 
			   close[\ ]*\( | common | continue | data | dimension | do | 
			   else | end | endfile | endif | entry | equivalence | 
			   external | format | function | goto | if | implicit | 
			   inquire | intrinsic | open[\ ]*\( | parameter | pause | 
			   print[\ ]+\( | program | read[\ ]*\( | return | rewind | 
			   rewrite | save[\ ]*\( | stop | subroutine | write[\ ]*\( | \( | \) | \[ | \] | \, | \:\:

FUNC1	     = ([^a-zA-Z0-9\_])?("acos" | "acosh" | "aimag" | "aint" | "anint" | "asin" | "asinh" | "atan" | "atan2" | "atanh" | "bessel_j0" | 
							  "bessel_j1" | "bessel_jn" | "bessel_y0" | "bessel_yn" | "cmplx" | "conjg" | "cos" | "cosh" | "dble" | "dprod" | 
							  "epsilon" | "erf" | "erfc" | "erfc_scaled" | "exp" | "fraction" | "gamma" | "hypot" | "log" | "log_gamma" | "log10" | 
							  "nearest" | "norm2" | "real" | "rrspacing" | "scale" | "set_exponent" | "sin" | "sinh" | "spacing" | "sqrt" | "tan" | 
							  "tanh" | "tiny" | "alog" | "alog10" | "amax0" | "amin0" | "amax1" | "amin1" | "amod" | "cabs" | "ccos" | "cexp" | 
							  "clog" | "csin" | "csqrt" | "dabs" | "dacos" | "dasin" | "datan" | "datan2" | "dcos" | "dcosh" | "ddim" | "dexp" | 
							  "dlog" | "dlog10" | "dmax1" | "dmin1" | "dmod" | "dpord" | "dsign" | "dsin" | "dsinh" | "dsqrt" | "dtan" | "dtanh" | 
							  "float"){SPACE}*"("
 
FUNC2        = ([^a-zA-Z0-9\_])?("abs" | "cshift" | "dim"   | "dot_product" | "eoshift" | "huge"  | "matmul" | "max" | "maxval" | "merge" | "min" | 
							  "minval" | "mod" |"pack" | "product" | "reshape" | "sign" | "spread" | "sum" | "transfer" | "transpose" | 
							  "unpack"){SPACE}*"("
FUNC3   	 =  ([^a-zA-Z0-9\_])?("achar" | "adjustl" | "adjustr" | "all" | "allocate" | "bit_size" | "ceiling" | "char" | "cmplx" | "command_argument_count" | 
							   "count" | "desallocate" | "digits" | "dshiftl" | "dshiftr" | "exponent" | "fraction" | "iachar" | "iall" | "iand" | "iany" | "iargc" |
							   "ibclr" | "ibits" | "ibset" | "ichar" | "ieor" | "image_index" | "index" | "int" | "ior" | "iparity" | "ishft" | 
							   "ishftc" | "kind" | " lbound" | "lcobound" | "leadz" | "len" | "len_trim" | "logical" | "maskl" | "maskr" | 
							   "maxexponent" | "maxloc" | "merge_bits" | "minexponent" | "minloc" | "modulo" | "new_line" | "nint" | "not" | 
							   "null" | "num_images" | "popcnt" | "poppar" | "radix" | "range" | "repeat" | "scan" | "selected_char_kind" | 
							   "selected_int_kind" | "selected_real_kind" | "shape" | "shifta" | "shiftl" | "shiftr" | "size" | "storage_size" | 
							   "this_image" | "trailz" | "trim" | "ubound" | "ucobound" | "verify" | "amin0" | "dint" | "dnint"){SPACE}*"("

VAREQ        = {SPACE}*{VAR}{SPACE}* (\= | \=\= | \!\= | \<\= | \>\= | ".EQ." | ".NE." | ".LE." | ".GE." | ".LT.", ".GT." |   ".eq." | ".ne." | ".le." | ".ge." | ".lt.", ".gt.")
TYPE1   	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision
TYPE2	     = "REAL" [\ ]* \(
TYPE3	     = "IMPLICIT" | "implicit"
TYPE4        = "INTENT" {SPACE}*({SPACE}*"OUT"{SPACE}*) | "INTENT" {SPACE}*({SPACE}*"IN"{SPACE}*) | "intent" {SPACE}*({SPACE}*"out"{SPACE}*) | "intent" {SPACE}*({SPACE}*"in"{SPACE}*) 
CLE			 = {CLE_F77} | {FUNC1} | {FUNC2} | {FUNC3} | {TYPE1} | {TYPE2} | {TYPE3} | {TYPE4} | {VAREQ}
   
END 		 = "END"
END_TYPE	 = {END} [\ ]+ {TYPE} 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t\f]
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMINSTCodeComment.class.getName());

	String location = "MAIN PROGRAM";
	List<String> loc = new LinkedList<String>();
    String parsedFileName;
	
	boolean endHeader = false;
	boolean params = false;
	
   //mantis 314
   // error number  
	 int nbError = 0;
   //total word of a comment line
     int nbTotal = 0;
   //percent of fault tolerance
     double rateLimit = 0.5;
	
	public COMINSTCodeComment(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(file.toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
%}

%eofval{
return getViolations();
%eofval}


%%          
				

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>		{CLE}			{
								 nbError++;
								 nbTotal++;
								}
<COMMENT>		{VAR}			{
								 nbTotal++;
								}
<COMMENT>		\!   			{}
<COMMENT>		\C  			{}
<COMMENT>		{SPACE}  		{}
<COMMENT>   	\n             	{
								 if(nbTotal >0) {
									 if((nbError/nbTotal)>rateLimit){
									 	LOGGER.fine("Setting error line "+(yyline+1)+" because commented code is not allowed. It shall be suppressed.");
                                 		setError(loc.get(loc.size()-1),"Commented code is not allowed. It shall be suppressed.", yyline+1);
      							 	}
      							 }
      							 nbError=0;
      							 nbTotal=0;
      							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
      							 yybegin(NEW_LINE);
								}  
<COMMENT>   	.              	{}            	


/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID> 	  	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }  
<AVOID>	   		.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    loc.add(location);
                                    endHeader = false;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> PARAMS (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(PARAMS);
                                }
<NAMING>    	\n             	{
                                    loc.add(location); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> HEADER (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(HEADER);
                                }
<YYINITIAL>		{TYPE}        	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    location = yytext();
                                    yybegin(NAMING);
                                    }
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                    }
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                    }


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    if(loc.size()>0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    } else {
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> HEADER (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                        yybegin(HEADER);
                                    }
                                }
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{END_TYPE}		{loc.remove(loc.size()-1);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>		\s				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : s )");
                                    yybegin(LINE);}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>		  	{FREE_COMMENT} 	{
                                    if(loc.size()>0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    } else {
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> HEADER (Transition : FREE_COMMENT \""+yytext()+"\" )");
                                        yybegin(HEADER);
                                    }
                                }
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>			{END_TYPE}		{loc.remove(loc.size()-1);}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>			{SPACE}			{endHeader=false;}
<LINE>      	.              	{}


/************************/
/* PARAMS STATE    	    */
/************************/
<PARAMS>		\(				{params = true;}
<PARAMS>		\)				{endHeader = true;}
<PARAMS>		\n				{
                                    if( (params && endHeader) || (!params) ) {
                                        params = false;
                                        endHeader = false;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PARAMS -> PREHEADER (Transition : \\n )");
                                        yybegin(PREHEADER);
                                    }
                                }
<PARAMS>		.				{}


/************************/
/* HEADER STATE    	    */
/************************/
<PREHEADER>		{COMMENT_WORD}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PREHEADER -> HEADER (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(HEADER);}
<PREHEADER>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PREHEADER -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<PREHEADER>		{SPACE} | \n	{}
<PREHEADER>		.				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PREHEADER -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }
<HEADER>		\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - HEADER -> HEADER_2 (Transition : \\n )");
                                    yybegin(HEADER_2);}
<HEADER>		.				{}
<HEADER_2>		{COMMENT_WORD}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - HEADER_2 -> HEADER (COMMENT_WORD : \\n )");
                                    yybegin(HEADER);}
<HEADER_2>		{TYPE}        	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - HEADER_2 -> NAMING (TYPE : \\n )");
                                    location = yytext();
                                    yybegin(NAMING);
                                }
<HEADER_2>		[^]			{
                                LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - HEADER_2 -> NEW_LINE (TYPE : [^] )");
                                yybegin(NEW_LINE);}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+(yyline+1)+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                               }