/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/******************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.FloatCompare rule*/
/* for F77.                                                                   */
/* For further information on this, we advise you to refer to RNC manuals.	  */
/* As many comments have been done on the ExampleRule.lex file, this file     */
/* will restrain its comments on modifications.								  */
/*																			  */
/******************************************************************************/



package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class COMDATAFloatCompare
%extends AbstractChecker
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* 4 new states are added :  					*/
/*   - INIT, to store all new variables. 		*/
/*	 - COMPARE, to deal with strict comparison. */
/*	 - BRACE, to deal with parenthesis.			*/
/*   - DECLARATION, to save all the variables that cause no error */
%state COMMENT, NAMING, INIT, COMPARE, BRACE, WAIT, DECLARATION, DECL_PARAM

COMMENT_LINE = \! [^\n]*
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* Two lists are created. The first one contains all declared variables. The second	*/
/* one stands for all variables in one instruction.								 	*/
/* A boolean called strictComp is set to determine if a strict comparison is made	*/
/* in the current analyzed instruction.												*/
%{
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	/** This list store all variables name declared in a function, program, etc. **/
	List<String> allVariables = new LinkedList<String>();
	List<String> allVariablesNoError = new LinkedList<String>();
	/** This list store all variables found inside parenthesis or function call. **/
	String currentVariable = "";
	/** This is used to determine if expression before == or /= is float expresions. **/
	boolean firstFloat = false;
	/** A boolean to determine if variables inside braces are ignored. **/
	boolean isIgnored = false;
	/** Integer to determine last closing brace. **/
	int brace = 0;
	boolean endLine = true;
	boolean canSetError = false;
	boolean added = false;
	int par = 0;
	
	public COMDATAFloatCompare() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	
%}

%eofval{
    
	
	return getCheckResults();
%eofval}

/* Transition word are  real, complex and double precision. Whenever this word is */
/* encountered, we store all variables declared.								  */ 
/* COMP words as /= or == are added.                                              */
/* Mantis 310 and 311 add where and forall as NOTHING 							 */
RULE_WORD    = ([^a-zA-Z0-9\_])?("real" | ("double"){SPACE}*("precision") | "complex")[^a-zA-Z0-9\_]
IMPLICIT	 = ([^a-zA-Z0-9\_])?("implicit")[^a-zA-Z0-9\_]
COMP		 = "/=" | "==" | ".eq." | ".ne."
NOTHING		 = ("if" | "elseif" | "forall" | "while" | "where" | "case"){SPACE}*"("
ERR_FUNC	 = ([^a-zA-Z0-9\_])?("acos" | "acosh" | "aimag" | "aint" | "anint" | "asin" | "asinh" | "atan" | "atan2" | "atanh" | "bessel_j0" | 
							  "bessel_j1" | "bessel_jn" | "bessel_y0" | "bessel_yn" | "cmplx" | "conjg" | "cos" | "cosh" | "dble" | "dprod" | 
							  "epsilon" | "erf" | "erfc" | "erfc_scaled" | "exp" | "fraction" | "gamma" | "hypot" | "log" | "log_gamma" | "log10" | 
							  "nearest" | "norm2" | "real" | "rrspacing" | "scale" | "set_exponent" | "sin" | "sinh" | "spacing" | "sqrt" | "tan" | 
							  "tanh" | "tiny" | "alog" | "alog10" | "amax0" | "amin0" | "amax1" | "amin1" | "amod" | "cabs" | "ccos" | "cexp" | 
							  "clog" | "csin" | "csqrt" | "dabs" | "dacos" | "dasin" | "datan" | "datan2" | "dcos" | "dcosh" | "ddim" | "dexp" | 
							  "dlog" | "dlog10" | "dmax1" | "dmin1" | "dmod" | "dpord" | "dsign" | "dsin" | "dsinh" | "dsqrt" | "dtan" | "dtanh" | 
							  "float"){SPACE}*"("
SEE_FUNC	 = ([^a-zA-Z0-9\_])?("abs"  | "cshift" | "dim"   | "dot_product" | "eoshift" | "huge"  | "matmul" | "max" | "maxval" | "merge" | "min" | 
							  "minval" | "mod" |"pack" | "product" | "reshape" | "sign" | "spread" | "sum" | "transfer" | "transpose" | 
							  "unpack"){SPACE}*"("
NO_ERR_FUNC	 =  ([^a-zA-Z0-9\_])?("achar" | "adjustl" | "adjustr" | "all" | "allocate" | "bit_size" | "ceiling" | "char" | "cmplx" | "command_argument_count" | 
							   "count" | "desallocate" | "digits" | "dshiftl" | "dshiftr" | "exponent" | "fraction" | "iachar" | "iall" | "iand" | "iany" | "iargc" |
							   "ibclr" | "ibits" | "ibset" | "ichar" | "ieor" | "image_index" | "index" | "int" | "ior" | "iparity" | "ishft" | 
							   "ishftc" | "kind" | " lbound" | "lcobound" | "leadz" | "len" | "len_trim" | "logical" | "maskl" | "maskr" | 
							   "maxexponent" | "maxloc" | "merge_bits" | "minexponent" | "minloc" | "modulo" | "new_line" | "nint" | "not" | 
							   "null" | "num_images" | "popcnt" | "poppar" | "radix" | "range" | "repeat" | "scan" | "selected_char_kind" | 
							   "selected_int_kind" | "selected_real_kind" | "shape" | "shifta" | "shiftl" | "shiftr" | "size" | "storage_size" | 
							   "this_image" | "trailz" | "trim" | "ubound" | "ucobound" | "verify" | "amin0" | "dint" | "dnint"){SPACE}*"("
FUNCTION	 = {VAR}{SPACE}*"("
DATA_TYPE	 = ("integer" | "logical" | "character" ) ( {SPACE} | {SPACE}*"(" | {SPACE}*"," | {SPACE}*"*" ) 

%%          

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   
		{
			\n|\r 		    {yybegin(YYINITIAL);}  
			.              	{}
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>    	
		{
			{COMMENT_LINE}	{}
			\n|\r	        {allVariables.clear(); 
							 yybegin(YYINITIAL);}
			{VAR}			{location = location + " " + yytext();
							 allVariables.clear(); 
							 yybegin(COMMENT);}
			.              	{}
		}

/*****************/ 
/*	INIT PART	 */
/*****************/	
<INIT>			
		{
			{COMMENT_LINE}	{}
			{FALSE}			{allVariables.add(yytext().toLowerCase());}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{VAR}			{allVariables.add(yytext().toLowerCase());} 
			\(				{par = 1; yybegin(WAIT);}
			.				{}
			\n|\r			{firstFloat = false; yybegin(YYINITIAL);}								 
		}
		
/*****************/
/*	WAIT PART	 */
/*****************/	
<WAIT>			
		{
			{COMMENT_LINE}	{}
			\(				{par++;}
			\)				{par--; if(par==0) yybegin(INIT);}
			.				{}
			\n|\r			{yybegin(YYINITIAL);}								 
		}
		
/**************************************/
/*	DECLARATION and DECL_PARAM PART	  */
/**************************************/	
<DECL_PARAM>			
		{
			{COMMENT_LINE}	{}
			\:\:			{yybegin(DECLARATION);}
			.				{}
			\n|\r			{yybegin(YYINITIAL);}								 
		}
<DECLARATION>			
		{
			{COMMENT_LINE}	{}
			{VAR}			{allVariablesNoError.add(yytext().toLowerCase());}
			\&				{endLine=false;}
			.				{}
			\n				{if(endLine)yybegin(YYINITIAL);
			                 endLine = true;}								 
		}
		
/*****************/
/*	BRACE STATE	 */
/*****************/	
<BRACE>			
		{
			{COMMENT_LINE}		{}
			{NO_ERR_FUNC}		{brace = brace + 1;}
			{ERR_FUNC}			{String var = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
								 if (!isIgnored) {
									if (canSetError && !added){
										this.setError(location,"It's not allowed to compare float variables (" + 
													  var + ") with equality." , yyline + 1);
									} else {
										currentVariable = var;
										firstFloat = true;
									}
								 }
								 brace = brace + 1;}
			{SEE_FUNC}			{brace = brace + 1;}
			{FUNCTION}			{String var = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
							 	 if(!allVariablesNoError.contains(var)) {
									 if (!isIgnored) {
										if (canSetError && !added){
											this.setError(location,"It's not allowed to compare float variables (" + 
													 var + ") with equality." , yyline + 1);
										} else {
											currentVariable = var;
											firstFloat = true;
										}
									 }
									 brace = brace + 1;
								 }
								}
			"("					{brace = brace + 1;}
			{VAR}				{if (!isIgnored) {
									if (allVariables.contains(yytext().toLowerCase())){
										if (canSetError && !added){
											this.setError(location,"It's not allowed to compare float variables (" + 
													 	  yytext() + ") with equality." , yyline + 1);
										} else {
											currentVariable = yytext().toLowerCase();
											firstFloat = true;
										}
									}
								 }
								}
			")"					{if (brace == 0){
									yybegin(YYINITIAL);
								 } else {
									brace = brace - 1;
								 }
								}
			"&"{SPACE}{1,5}[^\n\r]	{}
			"&"					{endLine = false;}
			.					{}
			\n|\r				{if (endLine) {
									added = false;
									firstFloat=false;
									yybegin(YYINITIAL);
								 }
								 endLine = true;
								}								 
		}
		
/********************/
/*	COMPARE STATE	*/
/********************/	
<COMPARE>			
		{	
			{COMMENT_LINE}	{}
			{NO_ERR_FUNC}	{isIgnored = true;
							 canSetError = false;
							 yybegin(BRACE);}
			{ERR_FUNC}		{isIgnored = true;
							 if (!added){
								this.setError(location,"It's not allowed to compare float variables (" + 
													  yytext() + ") with equality." , yyline + 1);
							 }
							 canSetError = false;
							 yybegin(BRACE);}
			{SEE_FUNC}		{isIgnored = false;
							 canSetError = true;
							 yybegin(BRACE);}
			{FUNCTION}		{String var = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
							 if(!allVariablesNoError.contains(var)) {
								 isIgnored = true;
								 if (!added){
									this.setError(location,"It's not allowed to compare float variables (" + 
													  var + ") with equality." , yyline + 1);
								 }
								 canSetError = false;
								 yybegin(BRACE);
							 }
							}
			"("				{isIgnored = false;
							 canSetError = true;
							 yybegin(BRACE);}
			{RULE_WORD}		{yybegin(INIT);}
			{VAR}			{if (!added){
								if (allVariables.contains(yytext().toLowerCase())) {
									this.setError(location,"It's not allowed to compare float variables (" + 
													  yytext() + ") with equality." , yyline + 1);
								}
							 }
							 added = false;
							 yybegin(YYINITIAL);
							}	
			{SPACE}+		{}	
			"&"				{}
			.				{yybegin(YYINITIAL);}
			\n|\r			{}
		}
		
/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>	
		{
			{COMMENT_LINE}	{}
			{STRING}		{}
			{FALSE}			{if (allVariables.contains(yytext().toLowerCase())){
								currentVariable = yytext().toLowerCase();
								firstFloat = true;
							 }
							}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{DATA_TYPE}		{yybegin(DECLARATION);}
			{IMPLICIT}		{yybegin(COMMENT);}
			{NOTHING}		{}
			/* Nothing is done for these functions */
			{NO_ERR_FUNC}	{isIgnored = true;
							 firstFloat = false;
							 canSetError = false;
							 yybegin(BRACE);}
			/* There might be an error with these functions */
			{ERR_FUNC}		{isIgnored = true;
							 firstFloat = true;
							 currentVariable = yytext().toLowerCase();
							 canSetError = false;
							 yybegin(BRACE);}
			/* We have to check arguments in these functions */
			{SEE_FUNC}		{isIgnored = false;
							 firstFloat = false;
							 canSetError = false;
							 yybegin(BRACE);}
			/* Unknown functions are considered as errors */
			{FUNCTION}		{String var = yytext().toLowerCase().substring(0, yytext().length()-1).trim();
							 if(!allVariablesNoError.contains(var)) {
							 	isIgnored = true;
							 	firstFloat = true;
							 	currentVariable = var;
							 	canSetError = false;
							 	yybegin(BRACE);
							 }
							}
			/* If it's an opening parenthesis, we check variables inside */
			"("				{isIgnored = false;
							 firstFloat = false;
							 canSetError = false;
							 yybegin(BRACE);}
			/* This is used to look for variable declaration */
			{RULE_WORD}		{yybegin(INIT);}
			/* If a variable is in wrong type, we set a potential error */
			{VAR}			{if (allVariables.contains(yytext().toLowerCase())){
								currentVariable = yytext().toLowerCase();
								firstFloat = true;
							 }
							}
			{SPACE}+		{}
			/* If these words have a potential error before, we throw an error */
			{COMP}			{if (firstFloat) {
								this.setError(location,"It's not allowed to compare float variables (" + 
											  currentVariable + ") with equality." , yyline + 1);
								firstFloat = false;
								added = true;
							 }
							 yybegin(COMPARE);}
			\n|\r		  	{firstFloat = false; currentVariable="";}
			.              	{firstFloat = false;}
		}

/*********************/
/*	ERROR THROWN	 */
/*********************/			
			[^]      	{throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}