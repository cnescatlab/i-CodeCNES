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
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMINSTCodeComment
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, PREHEADER, HEADER, HEADER_2, AVOID, PARAMS

COMMENT_WORD = \!
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
			   rewrite | save[\ ]*\( | stop | subroutine | write[\ ]*\(
CLE_F90		 = ALLOCATABLE | ALLOCATE[\ ]*\( | CASE | CONTAINS | CYCLE | 
			   DEALLOCATE[\ ]*\( | ELSEWHERE | EXIT | INCLUDE | INTERFACE | 
			   INTENT | MODULE | NAMELIST | NULLIFY[\ ]*\( | ONLY | OPERATOR | 
			   OPTIONAL | POINTER | PRIVATE | PROCEDURE | PUBLIC | RECURSIVE | 
			   RESULT | SELECT | SEQUENCE | TARGET | USE | WHILE | WHERE | 
			   allocatable | allocate[\ ]*\( | case | contains | cycle | 
			   deallocate[\ ]*\( | elsewhere | exit | include | interface | 
			   intent | module | namelist | nullify[\ ]*\( | only | operator | 
			   optional | pointer | private | procedure | public | recursive | 
			   result | select | sequence | target | use | while | where | \( | \) | \[ | \] | \, | \:\:
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
CLE			 = {CLE_F77} | {CLE_F90} | {FUNC1} | {TYPE1} | {TYPE2} | {TYPE3} | {VAREQ}
END 		 = "END"
END_TYPE	 = {END} [\ ]+ {TYPE} 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t\f]

																
%{
	String location = "MAIN PROGRAM";
	List<String> loc = new LinkedList<String>();
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
		this.zzReader = new FileReader(file.toOSString());
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
<COMMENT>		{FUNC2}			{
								 nbError++;
								 nbTotal++;
								}
<COMMENT>		{FUNC3}			{
								 nbError++;
								 nbTotal++;
								}
<COMMENT>		{VAR}			{

								 nbTotal++;
								}
<COMMENT>		\!   			{}
<COMMENT>		{SPACE}  		{}
<COMMENT>   	\n             	{
								 if(nbTotal >0) {
									 if((nbError/nbTotal)>rateLimit){	
                                 		setError(loc.get(loc.size()-1),"Commented code is not allowed. It shall be suppressed.", yyline+1);
      							 	}
      							 }
      							 nbError=0;
      							 nbTotal=0;
      							 yybegin(NEW_LINE);
								}  
<COMMENT>   	.              	{} 


/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID> 	  	\n             	{yybegin(NEW_LINE);}  
<AVOID>	   		.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{location = location + " " + yytext();  loc.add(location); endHeader = false; yybegin(PARAMS);}
<NAMING>    	\n             	{loc.add(location); yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(HEADER);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>		\!\$			{yybegin(HEADER);}
<NEW_LINE>  	{COMMENT_WORD} 	{if(loc.size()>0)yybegin(COMMENT); else yybegin(HEADER);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{END_TYPE}		{loc.remove(loc.size()-1);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>		\s				{yybegin(LINE);}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>		  	{COMMENT_WORD} 	{if(loc.size()>0)yybegin(COMMENT); else yybegin(HEADER);}
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{END_TYPE}		{loc.remove(loc.size()-1);}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>			{SPACE}			{endHeader=false;}
<LINE>      	.              	{}


/************************/
/* PARAMS STATE    	    */
/************************/
<PARAMS>		\(				{params = true;}
<PARAMS>		\)				{endHeader = true;}
<PARAMS>		\n				{if( (params && endHeader) || (!params) ) {params = false; endHeader = false; yybegin(PREHEADER);}}
<PARAMS>		.				{}


/************************/
/* HEADER STATE    	    */
/************************/
<PREHEADER>		{COMMENT_WORD}	{yybegin(HEADER);}
<PREHEADER>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<PREHEADER>		{SPACE} | \n	{}
<PREHEADER>		.				{yybegin(LINE);}
<HEADER>		\n				{yybegin(HEADER_2);}
<HEADER>		.				{}
<HEADER_2>		{COMMENT_WORD}	{yybegin(HEADER);}
<HEADER_2>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<HEADER_2>		[^] 			{yybegin(NEW_LINE);}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}