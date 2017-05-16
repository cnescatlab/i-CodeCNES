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



package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

%%

%class COMDATAFloatCompare
%extends AbstractRule
%public
%line
%ignorecase
%column


%function run
%yylexthrow JFlexException
%type List<Violation>

/* 3 new states are added :  					*/
/*   - INIT, to store all new variables. 		*/
/*	 - COMPARE, to deal with strict comparison. */
/*	 - BRACE, to deal with parenthesis.			*/
%state COMMENT, NAMING, INIT,INITOTHER, COMPARE, BRACE, WAIT, INITOTHER

FREE_COMMENT = "!"
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
    private static final Logger LOGGER = Logger.getLogger(COMDATAFloatCompare.class.getName());
	
	
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	/** This list store all variables name declared in a function, program, etc. **/
	List<String> allVariables = new LinkedList<String>();
	/** mantis 309/310 07/08/2015 This list store all other variables name declared in a function, program, etc. not concerned by this rule **/
	List<String> allOtherVariables = new LinkedList<String>();
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
	String parsedFileName;
	

	
	public COMDATAFloatCompare() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        this.parsedFileName = file.toString();
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	
%}

%eofval{
    
	
	return getViolations();
%eofval}

/* Transition word are  real, complex and double precision. Whenever this word is */
/* encountered, we store all variables declared.								  */ 
/* COMP words as /= or == are added.											  */
/* Mantis 310 and 311 add where and forall as NOTHING										     	  */
RULE_WORD    = ([^a-zA-Z0-9\_])?("real" | ("double"){SPACE}*("precision") | "complex")[^a-zA-Z0-9\_]
NO_RULE_WORD    = ([^a-zA-Z0-9\_])?("integer")[^a-zA-Z0-9\_]
IMPLICIT	 = ([^a-zA-Z0-9\_])?("implicit")[^a-zA-Z0-9\_]
COMP		 = "/=" | "==" | ".eq." | ".ne."
NOTHING		 = ([^a-zA-Z0-9\_])?("if" | "elseif" | "forall" | "while" | "where" ){SPACE}*"("
ERR_FUNC	 = ([^a-zA-Z0-9\_])?("acos" | "acosh" | "aimag" | "aint" | "anint" | "asin" | "asinh" | "atan" | "atan2" | "atanh" | "bessel_j0" | 
							  "bessel_j1" | "bessel_jn" | "bessel_y0" | "bessel_yn" | "cmplx" | "conjg" | "cos" | "cosh" | "dble" | "dprod" | 
							  "epsilon" | "erf" | "erfc" | "erfc_scaled" | "exp" | "fraction" | "gamma" | "hypot" | "log" | "log_gamma" | "log10" | 
							  "nearest" | "norm2" | "real" | "rrspacing" | "scale" | "set_exponent" | "sin" | "sinh" | "spacing" | "sqrt" | "tan" | 
							  "tanh" | "tiny" | "alog" | "alog10" | "amax0" | "amin0" | "amax1" | "amin1" | "amod" | "cabs" | "ccos" | "cexp" | 
							  "clog" | "csin" | "csqrt" | "dabs" | "dacos" | "dasin" | "datan" | "datan2" | "dcos" | "dcosh" | "ddim" | "dexp" | 
							  "dlog" | "dlog10" | "dmax1" | "dmin1" | "dmod" | "dpord" | "dsign" | "dsin" | "dsinh" | "dsqrt" | "dtan" | "dtanh" | 
							  "float"){SPACE}*"("
 
SEE_FUNC	 = ([^a-zA-Z0-9\_])?("abs" | "cshift" | "dim"   | "dot_product" | "eoshift" | "huge"  | "matmul" | "max" | "maxval" | "merge" | "min" | 
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

%%          
/*************************/
/*	FREE COMMENT CATCH	 */
/*************************/
			{FREE_COMMENT}	{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT)");
                			     yybegin(COMMENT);
                			}

/*********************/
/*	COMMENT PART	 */
/*********************/
<COMMENT>   
		{
			\n|\r 		    {
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> YYINITIAL (Transition : \\n | \\r )");
                    			yybegin(YYINITIAL);
                			}  
			.              	{}
		}

/*****************/
/*	NAMING PART	 */
/*****************/
<NAMING>    	
		{
			\n|\r	        {
                    			allVariables.clear(); 
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> YYINITIAL (Transition : \\n | \\r )");
                                yybegin(YYINITIAL);
                		    }
			{VAR}			{
    			                 location = location + " " + yytext();
    							 allVariables.clear();
    							 //mantis 309/310 07/08/2015
    							 allOtherVariables.clear();
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR )");
                                 yybegin(COMMENT);
    					    }
			.              	{}
		}

/*****************/
/*	INIT PART	 */
/*****************/	
<INIT>			
		{
			{FALSE}			{
			                     allVariables.add(yytext());
			                }
			{TYPE}         	{
			                     location = yytext();
							     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> NAMING (Transition : TYPE )");
                                 yybegin(NAMING);
							}
			{VAR}			{
                			     allVariables.add(yytext());
                			} 
			\(				{
                    			par = 1; 
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> WAIT (Transition : [(] )");
                                yybegin(WAIT);
                			}
			.				{}
			\n|\r			{
                			    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> YYINITIAL (Transition : \\n | \\r )");
                                yybegin(YYINITIAL);
                			}								 
		}

/*****************/
/*INITOTHER PART */
/*****************/	
<INITOTHER>			
		{
			{FALSE}			{
                			     allOtherVariables.add(yytext());
                			}
			{TYPE}         	{
    			                 location = yytext(); 
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INITOTHER -> NAMING (Transition : TYPE )");
                                 yybegin(NAMING);
							}
			{VAR}			{
                			     allOtherVariables.add(yytext());
                			} 
			\(				{
                			     par = 1; 
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INITOTHER -> WAIT (Transition : [(] )");
                			     yybegin(WAIT);
                			}
			.				{}
			\n|\r			{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INITOTHER -> YYINITIAL (Transition : \\n | \\r )");
                			     yybegin(YYINITIAL);
                			}								 
		}
		
/*****************/
/*	WAIT PART	 */
/*****************/	
<WAIT>			
		{
			\(				{
                			     par++;
                			}
			\)				{
                    			par--; 
                    			if(par==0){
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT -> INIT (Transition : [)] && par ==0)");
                    			     yybegin(INIT);
                    			}
                			}
			.				{}
			\n|\r			{
    			                 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT -> YYINITIAL (Transition : \\n | \\r)");
    			                 yybegin(YYINITIAL);
    			             }								 
		}
		
/*****************/
/*	BRACE STATE	 */
/*****************/	
<BRACE>			
		{
			{NO_ERR_FUNC}		{
                    			     brace = brace + 1;
                    			}
			{ERR_FUNC}			{
    		                         if (!isIgnored) {
    									if (canSetError && !added){
    										this.setError(location, "It's not allowed to compare float variables (" + 
    													  yytext().replace("(","") + ") with equality." , yyline + 1);
    									} else {
    										currentVariable = yytext();
    										firstFloat = true;										
    									}
    								 }
    								 brace = brace + 1;
								}
			{SEE_FUNC}			{
                    			     brace = brace + 1;
                    			}
			{FUNCTION}			{
								 //mantis 309/310 if this function is not an other array variable
							     if(!allOtherVariables.contains(yytext().replace("(","").trim())){
								  if (!isIgnored) {
									if (canSetError && !added){
										this.setError(location, "It's not allowed to compare float variables (" + 
													  yytext().replace("(","") + ") with equality." , yyline + 1);
									} else {
										currentVariable = yytext();
										firstFloat = true;										
									}
								 }
								 
								 brace = brace + 1;}}
			"("					{
                        			brace = brace + 1;
                    			}
			{VAR}				{
    			                     if (!isIgnored) {
    									if (allVariables.contains(yytext())){
    										if (canSetError && !added){
    											this.setError(location,"It's not allowed to compare float variables  (" + 
    														  yytext().replace("(","") + ") with equality." , yyline + 1);
    										} else {
    											currentVariable = yytext();
    											firstFloat = true;
    										}
    									}
    								 }
								 
								}
			")"					{
    			                     if (brace == 0){
    			                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - BRACE -> YYINITIAL (Transition : [)] && brace == 0 )");
    									yybegin(YYINITIAL);
    								 } else {
    									brace = brace - 1;
    								 }
								}
			"&"{SPACE}*[^\n\r]	{}
			"&"					{
			endLine = false;
			}
			.					{}
			\n|\r				{
    			                     if (endLine) {
    									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - BRACE -> YYINITIAL (Transition : (\\n | \\r) && endLine = true )");
    									yybegin(YYINITIAL);
    									added = false;
    								 }
    								 endLine = true;
								 
								}								 
		}
		
/********************/
/*	COMPARE STATE	*/
/********************/	
<COMPARE>			
		{
			{NO_ERR_FUNC}	{
    			                 isIgnored = true;
    							 canSetError = false;							
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> BRACE (Transition : NO_ERR_FUNC )");
    							 yybegin(BRACE);
							 }
			{ERR_FUNC}		{
    			                 isIgnored = true;
    							 if (!added){
    								this.setError(location,"It's not allowed to compare float variables  (" + 
    											  yytext().replace("(","") + ") with equality." , yyline + 1);
    							 }
    							 canSetError = false;							 
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> BRACE (Transition : ERR_FUNC )");
    							 yybegin(BRACE);
							}
			{SEE_FUNC}		{
    			                 isIgnored = false;
    							 canSetError = true;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> BRACE (Transition : SEE_FUNC )");						 
    							 yybegin(BRACE);
    					    }
			{FUNCTION}		{
    							//mantis 309/310 if this function is not an other array variable
    							 if(!allOtherVariables.contains(yytext().replace("(","").trim())){
    								 isIgnored = true;
        							 if (!added){
        								this.setError(location,"It's not allowed to compare float variables  (" + 
        											  yytext().replace("(","") + ") with equality." , yyline + 1);
        							 }
        							 canSetError = false;
        							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> BRACE (Transition : FUNCTION )"); 						  
        							 yybegin(BRACE);
    							 }
							}
			"("				{
    			                 isIgnored = false;
    							 canSetError = true;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> BRACE (Transition : [(] )");     
    							 yybegin(BRACE);
							 }
			{NO_RULE_WORD}	{ 
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> INITOTHER (Transition : NO_RULE_WORD )");
                			     yybegin(INITOTHER);
                			}
			{RULE_WORD}		{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> INIT (Transition : RULE_WORD )");
                			     yybegin(INIT);
                			}
			{VAR}			{
    			                 if (!added){
    								if (allVariables.contains(yytext())) {
    									this.setError(location,"It's not allowed to compare float variables  (" + 
    												  yytext().replace("(","") + ") with equality." , yyline + 1);
    								}
    							 }
    							 added = false;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> YYINITIAL (Transition : VAR )");							 
    							 yybegin(YYINITIAL);
							}	
			{SPACE} 		{}	
			"&"				{}
			.				{
                    			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMPARE -> YYINITIAL (Transition : . )");
                    			yybegin(YYINITIAL);
                			}
			\n|\r			{}
		}
		
/*********************/
/*	INITIAL STATE	 */
/*********************/
<YYINITIAL>	
		{
			{STRING}		{}
			{FALSE}			{
    			                 if (allVariables.contains(yytext())){
    								currentVariable = yytext();
    								firstFloat = true;
    							 }
							 
							}
			{TYPE}         	{
			                     location = yytext(); 
							     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE )");
							     yybegin(NAMING);
						     }
			{IMPLICIT}		{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : IMPLICIT )");
                			     yybegin(COMMENT);
                			}
			{NOTHING}		{}
			/* Nothing is done for these functions */
			{NO_ERR_FUNC}	{
    			                 isIgnored = true;
    							 firstFloat = false;
    							 canSetError = false;							 
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BRACE (Transition : NO_ERR_FUNC )");
    							 yybegin(BRACE);
							 }
			/* There might be an error with these functions */
			{ERR_FUNC}		{
    			                 isIgnored = true;
    							 firstFloat = true;
    							 currentVariable = yytext();
    							 canSetError = false;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BRACE (Transition : ERR_FUNC )");							  
    							 yybegin(BRACE);
							}
			/* We have to check arguments in these functions */
			{SEE_FUNC}		{
    			                 isIgnored = false;
    							 firstFloat = false;
    							 canSetError = false;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BRACE (Transition : SEE_FUNC )");							 
    							 yybegin(BRACE);
							 }
			/* Unknown functions are considered as errors */
			{FUNCTION}		{
    							  //mantis 309/310 if this function is not an other array variable
    							  if(!allOtherVariables.contains(yytext().replace("(","").trim())){
    			 					isIgnored = true;
    							 	firstFloat = true;							
    							 	currentVariable = yytext();
    							 	canSetError = false;
    						 	   }							  	
    							   LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BRACE (Transition : FUNCTION )");
    							   yybegin(BRACE);
						      }
			/* If it's an opening parenthesis, we check variables inside */
			"("				{
    			                 isIgnored = false;
    							 firstFloat = false;
    							 canSetError = false;
    							 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> BRACE (Transition : [(] )");							   
    							 yybegin(BRACE);
							 }
			/* This is used to look for variable declaration */
			{RULE_WORD}		{
                			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> INIT (Transition : RULE_WORD )");
                			     yybegin(INIT);
                			}
			{NO_RULE_WORD}		{ 
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> INITOTHER (Transition : NO_RULE_WORD )");
                    			     yybegin(INITOTHER);
                    			}
			/* If a variable is in wrong type, we set a potential error */
			{VAR}			{
			                     if (allVariables.contains(yytext())){
        								currentVariable = yytext();
        								firstFloat = true;
							     }							 
							}
			{SPACE} 		{}
			/* If these words have a potential error before, we throw an error */
			{COMP}			{
			                     if (firstFloat) {
    								this.setError(location,"It's not allowed to compare float variables (" + 
    											  currentVariable.replace("(","") + ") with equality." , yyline + 1);
    								firstFloat = false;
    								added = true;
						         }							 
							     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMPARE (Transition : COMP )");
							     yybegin(COMPARE);
						    }
			\n|\r		  	{
			                     firstFloat = false;
			                }
			.              	{
                			     firstFloat = false;
                			}
		}

/*********************/
/*	ERROR THROWN	 */
/*********************/			
			[^]      	{
                                    String errorMessage = "Class"+this.getClass().getName()+" \nIllegal character <" + yytext() + "> \nFile :"+ this.parsedFileName+" \nat line:"+yyline+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                        }