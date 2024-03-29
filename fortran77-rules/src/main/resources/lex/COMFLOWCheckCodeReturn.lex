/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.FLOW.CheckCodeReturn rule.*/
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.icode.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMFLOWCheckCodeReturn
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* We add 5 states to the initial ones : 													*/
/*    - EQUAL, which is used when an equal after a variable is foundd (like VAR = ...)		*/
/*    - IF, which is used when an if or a select cas statement is encountered				*/
/*    - VAR_EQ, which is used when a variable is encountered at the beginning of a line,	*/
/*      to determine if an equal is folowing it												*/
/*    - DECL, which is used to determine if variables are declared							*/
/*    - CONT, used to determine if a line is a continuation of another.                     */
%state NEW_LINE, COMMENT, NAMING, EQUAL, IF, VAR_EQ, DECL

COMMENT_WORD = "!" | "c" | "*"
FREE_COMMENT = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]
SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* When add 10 words to the initials ones :				*/
/*    - IF, to catch if and select case statement		*/
/*    - EQUAL, to catch "="								*/
/*    - OPER, to catch +, -, / and *					*/
/*    - DECL, to catch declaration (integer, etc)		*/
/*	  - IMPLICIT, to avoid implicit declaration			*/
/*    - THEN, to catch then word						*/
/*    - ELSE, to catch else word						*/
/*    - CASE, to catch case word                     	*/
/*	  - DO, to catch do word                           	*/
/*    - END, to catch end statement						*/
/*    - AVOIDED, to catch intrinsic fortran functions	*/

IF			 = [^a-zA-Z0-9\_]("if" | ("select")({SPACE}*)("case"))[^a-zA-Z0-9\_]
EQUAL        = "="
OPER         = "+" | "-" | "/" | "*"
DECL		 = [^a-zA-Z0-9\_]("integer" | ("double"){SPACE}*("precision") | "real" | "character" | "complex" | "logical" | "parameter" | "dimension")[^a-zA-Z0-9\_]
IMPLICIT     = [^a-zA-Z0-9\_]("implicit")[^a-zA-Z0-9\_]
THEN         = [^a-zA-Z0-9\_]("then")[^a-zA-Z0-9\_]
ELSE 		 = [^a-zA-Z0-9\_]("else"){SPACE}*("if")?[^a-zA-Z0-9\_]
CASE		 = [^a-zA-Z0-9\_]("case")[^a-zA-Z0-9\_]
DO			 = [^a-zA-Z0-9\_]("do")[^a-zA-Z0-9\_]
END			 = [^a-zA-Z0-9\_]("end"){SPACE}*( "if" | "select" | "do" | {TYPE} )?{SPACE}*(\n|\r)*
AVOIDED		 = {SPACE}*( "abs" | "achar" | "acos" | "acosh" | "adjustl" | "adjustr" | "aimag" | "aint" | "all" | "anint" | "any" |
						 "asin" | "asinh" | "atan" | "atan2" | "atanh" | "bessel_j0" | "bessel_j1" | "bessel_jn" | "bessel_y0" |
						 "bessel_yn" | "bge" | "bgt" | "ble" | "blt" | "btest" | "ceiling" | "char" | "cmplx" |
						 "command_argument_count" | "conjg" | "cos" | "count" | "cshift" | "dble" | "dot_product" | "dpord" |
						 "dshiftl" | "dshiftr" | "eoshift" | "erf" | "erfc" | "erfc_scaled" | "exp" | "exponent" | "findloc" |
						 "floor" | "fraction" | "gamma" | "hypot" | "iachar" | "iall" | "iand" | "iany" | "ibclr" | "ibits" |
						 "ibset" | "ichar" | "ieor" | "index" | "int" | "ior" | "iparity" | "ishft" | "ishftc" | "is_iostat_end" |
						 "is_iostat_eor" | "leadz" | "len_trim" | "lge" | "lgt" | "lle" | "llt" | "log" | "lo_gamma" | "log10" |
						 "logical" | "maskl" | "maskr" | "matmul" | "max" | "" | "maxloc" | "maxval" | "merge" | "merge_bits" |
						 "min" | "minloc" | "minval" | "mod" | "modulo" | "nearest" | "nint" | "norm2" | "not" | "null" |
						 "num_images" | "pack" | "parity" | "popcnt" | "poppar" | "product" | "real" | "repeat" | "reshape" |
						 "rrspacing" | "scale" | "scan" | "selected_char_kind" | "selected_int_kind" | "selected_real_kind" |
						 "set_exponent" | "shifta" | "shiftl" | "shiftr" | "sign" | "sin" | "sinh" | "spacing" | "spread" |
						 "sqrt" | "sum" | "tan" | "tanh" | "this_image" | "trailz" | "transfer" | "transpose" | "trim" | "unpack" |
						 "verify" | "alog" | "alog10" | "amax0" | "amax1" | "amin0" | "amin1" | "amod" | "cabs" | "ccos" | "cexp" |
						 "clog" | "csin" | "csqrt" | "dabs" | "dacos" | "dasin" | "datan" | "datan2" | "dcos" | "dcosh" | "ddim" |
						 "dexp" | "dint" | "dlog" | "dlog10" | "dmax1" | "dmin1" | "dmod" | "dnint" | "dpord" | "dsign" | "dsin" |
						 "dsinh" | "dsqrt" | "dtan" | "dtanh" | "float" | "iabs" | "ichar" | "idim" | "idint" | "ifix" | "isign" |
						 "max0" | "max1" | "min0" | "min1" | "sngl" ){SPACE}*\(
		
		
%{
	/** Logger **/
	private static final Logger LOGGER = Logger.getLogger(COMFLOWCheckCodeReturn.class.getName());
	
	
	/** Variable used to store violation location and variable involved. **/
	String location = "MAIN PROGRAM";
	/** Variable used to store file value and function values associated. **/
	
	/** Boolean used to determine if a line continues or not. **/
	boolean ampFound = false;
	/** List of String containing all variables associated to function return. **/
	List<String> variables = new LinkedList<String>();
	/** List of locations associated to previous variables. **/
	List<String> locations = new LinkedList<String>();
	/** List of lines associated to previous variables. **/
	List<Integer> lines = new LinkedList<Integer>();
	/** A String used to store variables encountered. **/
	String variable = "";
	/** A boolean which is set to true if OPER is encountered. **/
	boolean isOper = false;
	/** A boolean set to true when a variable is added to the list. **/
	boolean added = false;
	/** List of String containing all declared variables, to avoid mistaking tab for a function. **/
	List<String> possibleTab = new LinkedList<String>();
	/** List of String to identify which IF statement a variable is in. **/
	List<LinkedList<Integer>> codeLevels = new LinkedList<LinkedList<Integer>>();
	/** An integer to determine the current IF level. **/
	LinkedList<Integer> codeLevel = new LinkedList<Integer>();
	/** A boolean to determine if an else statement if found. **/
	boolean elseFound = false;
	/** Name of the file being parsed*/
	String parsedFileName;
	
	
	
    public COMFLOWCheckCodeReturn() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
		codeLevel.add(1);
        LOGGER.finest("end method setInputFile");
	}
	
	/**
	 * Method used to raise all violations remaining at the end of a function, subroutine, etc.
	 * All variables remaining in variables list are considered as error (they are deleted when
	 * found in an if or select case statement).
	 */
	private void raiseRemainingErrors() throws JFlexException {
        LOGGER.finest("begin method raiseRemainingErrors");
		for (int i =0; i < variables.size(); i++){
			LOGGER.fine("Setting error line "+lines.get(i)+" for the variable "+  variables.get(i)+".");
			setError(locations.get(i),"The return code of the function "+ variables.get(i) + " is not checked.", lines.get(i));
		}
		variables.clear();
		locations.clear();
		lines.clear();
		codeLevels.clear();
		codeLevel.clear();
        LOGGER.finest("end method raiseRemainingErrors");
	}
	
	/**
	 * Method used to sort violations on increasing order considering their lines.
	 **/
	private void sortResults(){
        LOGGER.finest("begin method sortResults");
		Collections.sort(getCheckResults(), new Comparator<CheckResult>() {
         @Override
         public int compare(CheckResult o1, CheckResult o2) {
             int comparison = 0;
			 if (o1.getLine() < o2.getLine()){
				comparison = -1;
			 }
			 if (o1.getLine() > o2.getLine()){
				comparison = 1;
			 }
			 return comparison;
         }
     });
        LOGGER.finest("end method sortResults");
	}
	
	/**
	 * Method used to determine is variable is a declared tab.
	 * @param text
	 *			the variable to analyze
	 * @return true if the variable is a tab, flase otherwise
	 **/
	private boolean isTab(String text){
        LOGGER.finest("begin method isTab");
		boolean tab = false;
		for (String element : possibleTab){
				if (text.replace(" ","").replace("(","").equals(element)){
					tab = true;
				}
		}
        LOGGER.finest("end method isTab");
		return tab;
	}
	
	/**
	 * Method to verify that there is no conflict by writing on the same variable.
	 * This takes to list of integer for parameter, the old level and new one. If
	 * two writing in the same variable are in to else process independent (which 
	 * means depending on the same first if condition) then there's no conflict.
	 * 
	 * @param oldLevel
	 *				the list of integer for old affectation 
	 * @param newLevel
	 *				the list of integer for new affectation
	 * @return true if there's a conflicting for these affectations, false otherwise
	 **/
	private boolean isConflictLevel(List<Integer> oldLevel, List<Integer> newLevel){
        LOGGER.finest("begin method isTab");
		boolean conflict = true;
		int size = Math.min(oldLevel.size(), newLevel.size());
		if (size > 1 || oldLevel.get(0) == newLevel.get(0)){
			int i = 1;
			while (conflict && i < size){
				if (oldLevel.get(i) != newLevel.get(i)){
					conflict = false;
				}
				i = i+1;
			}
		}
        LOGGER.finest("end method isTab");
		return conflict;
	}
		
%}

%eofval{
	raiseRemainingErrors();
	sortResults();
return getCheckResults();
%eofval}
%eofclose


%%          

/************************/
			{FREE_COMMENT}		{
                        			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
                        			yybegin(COMMENT);
                    			}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
			\n|\r           	{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n|\\r )");
                    			     yybegin(NEW_LINE);
                    			}
			.              		{}
		}

/************************************************************************/
/* NAMING STATE	        												*/
/*																		*/
/* At this state, we raise all remaining errors from the previous part, */
/* and clear the possibleTab list.										*/
/************************************************************************/
<NAMING>		
		{
			{VAR}				{possibleTab.clear(); 
								 location = location + " " + yytext(); 
								 raiseRemainingErrors();
								 codeLevel.add(1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
								 yybegin(COMMENT);}
			(\n|\r)+          	{possibleTab.clear(); 
								 raiseRemainingErrors(); 
								 codeLevel.add(1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n|\\r )");
								 yybegin(NEW_LINE);}
		}

		
/****************************************************************/
/* DECL STATE           										*/
/*																*/
/* When a variable is found, it is stored in possibleTab list.	*/
/* We avoid everything after by switching to AVOID.				*/
/****************************************************************/
<DECL>		
		{
			{TYPE}				{location = yytext();
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
								 yybegin(NAMING);}
			{VAR}				{possibleTab.add(yytext());}
			.					{}
			(\n|\r)+			{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECL -> NEW_LINE (Transition : \\n|\\r )");
                    			     yybegin(NEW_LINE);
                    			}
		}

/****************************************************************************************************/
/* VAR_EQ STATE         																			*/
/*																									*/
/* Nothing is done when a space is found. EQUAL makes a move to EQUAL state, else we go to COMMENT. */
/****************************************************************************************************/
<VAR_EQ>					
		{	
			{EQUAL} 			{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR_EQ -> EQUAL (Transition : EQUAL \""+yytext()+"\" )");
                    			     yybegin(EQUAL);
                    			}
			{SPACE}				{}
			.					{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR_EQ -> COMMENT (Transition : SPACE \""+yytext()+"\" )");
                    			     yybegin(COMMENT);
                    			}
		}


/************************************************************************************************/
/* EQUAL STATE          																		*/
/*																								*/
/* All avoided functions do nothing. OPER sets isOper to true.									*/
/* When VAR{SPACE}*\( structure is found, we do the following checks :							*/
/*    - verify that catch word is not a tab and variable as not already been added				*/
/*    - if not, we check if variable is already in variables list								*/
/*    - if it is, when send an error and remove variable, location and line corresponding		*/
/*    - we add new location, line and variable 													*/
/* When multiple \n or \r are found, we check that :											*/
/*    - isOper is true																			*/
/*    - variables is not empty																	*/
/*    - last variables element is equal to variable												*/
/* If all conditions are true, we raise a violation and remove the last element of variables.	*/
/* We set isOper and added to false and begin NEW_LINE.											*/
/************************************************************************************************/
<EQUAL>			
		{
			{STRING}					{}
			{AVOIDED}					{}
			"&"{SPACE}*[^\n\r]			{}
			"&"							{ampFound = true;}
			{OPER}						{isOper = true;}
			{SPACE}*{VAR}{SPACE}*\( 	{if (!isTab(yytext()) && !added){
											if (variables.contains(variable)){
												LinkedList<String> tempVar = new LinkedList<String>();
												LinkedList<Integer> tempLine = new LinkedList<Integer>();
												LinkedList<String> tempLoc = new LinkedList<String>();
												LinkedList<LinkedList<Integer>> tempLevel = new LinkedList<LinkedList<Integer>>();
												int i = 0;
												for(String var : variables){
													if (var.equals(variable) && isConflictLevel(codeLevels.get(i), codeLevel)) {
													    LOGGER.fine("Setting error line "+lines.get(i)+" for the variable "+ variables.get(i).toUpperCase() +".");
														setError(locations.get(i),"The return code of the function "+ variables.get(i).toUpperCase() + " is not checked.", lines.get(i));
													} else {
														tempVar.add(var);
														tempLevel.add(codeLevels.get(i));
														tempLoc.add(locations.get(i));
														tempLine.add(lines.get(i));
													}
													i = i + 1;
												}
												variables = (List<String>) tempVar.clone();
												lines = (List<Integer>) tempLine.clone();
												locations = (List<String>) tempLoc.clone();
												codeLevels = (List<LinkedList<Integer>>) tempLevel.clone();
											}
											variables.add(variable);
											codeLevels.add((LinkedList<Integer>) codeLevel.clone());
											locations.add(location);
											lines.add(yyline + 1);
											added = true;
										 }
										}
			(\n|\r)+					{if (isOper && !variables.isEmpty() && variables.get(variables.size()-1).equals(variable)){
												LOGGER.fine("Setting error line "+(yyline+1)+" for returning code used in arithmetical statement.");
												setError(location,"Return code used in arithmetical statement.", yyline + 1);
												locations.remove(variables.size()-1);
												lines.remove(variables.size()-1);
												variables.remove(variables.size()-1);
										 }
										 if (!ampFound){
											isOper = false;
											added = false;
											LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - EQUAL -> NEW_LINE (Transition : \\n|\\r)");
											yybegin(NEW_LINE);
										 }
										 ampFound = false;
										}
		}

/****************************************************************************************/
/* IF STATE             																*/
/* 																						*/
/* We compare every variable found to the variables list. If it is in, we remove it. 	*/
/****************************************************************************************/
<IF>		{VAR} 				{variable = yytext();
								 if (variables.contains(variable) ){
									LinkedList<String> tempVar = new LinkedList<String>();
									LinkedList<Integer> tempLine = new LinkedList<Integer>();
									LinkedList<String> tempLoc = new LinkedList<String>();
									LinkedList<LinkedList<Integer>> tempLevel = new LinkedList<LinkedList<Integer>>();
									int i = 0;
									for(String var : variables){
										if (!var.equals(variable) || !isConflictLevel(codeLevels.get(i), codeLevel)) {
											tempVar.add(var);
											tempLevel.add(codeLevels.get(i));
											tempLoc.add(locations.get(i));
											tempLine.add(lines.get(i));
										}
										i = i + 1;
									}
									variables = (List<String>) tempVar.clone();
									lines = (List<Integer>) tempLine.clone();
									locations = (List<String>) tempLoc.clone();
									codeLevels = (List<LinkedList<Integer>>) tempLevel.clone();
								 }
								}
			{THEN}				{if (!elseFound){
									codeLevel.add(1);
								 }
								}
			"&"{SPACE}*[^\n\r]	{}
			"&"					{ampFound = true;}
			(\n|\r)+	        {if (!ampFound){
									elseFound = false;
									LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IF -> YYINITIAL (Transition : \\n|\\r)");
									yybegin(YYINITIAL);
								 }
								 ampFound = false;
								}

/********************************************************************************/
/* GENERIC BEHAVIOR 															*/	
/* 																				*/
/* For each original state :													*/
/*    - a String makes a move to LINE state (also for EQUAL state)				*/
/*    - FALSE is used to avoid variables with type names contained,				*/
/*      it also makes a move to VAR_EQ state (an store text into variable)		*/
/*    - TYPE is used for functions declarations									*/
/*    - DECL makes a move to DECL state											*/
/*    - IF makes a move to IF state												*/
/*    - VAR makes a move to VAR_EQ state (an store text into variable)			*/
/*    - SPACE makes a move to LINE state										*/
/*    - Anything else (except \n or \r) makes a move to AVOID state				*/
/*    - Multiple \n or \r makes a move to YYINITIAL state (as for DECL, VAR_EQ	*/
/*      and IF state).															*/
/********************************************************************************/
<YYINITIAL>
		{
								{STRING}			{}
								{FALSE}				{variable = yytext(); 
													 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> VAR_EQ (Transition : FALSE \""+yytext()+"\")");
													 yybegin(VAR_EQ);}
								{TYPE}        		{location = yytext(); 
													 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\")");
													 yybegin(NAMING);}
								{IMPLICIT}			{}
								{DECL}				{
								                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> DECL (Transition : DECL \""+yytext()+"\")");
								                        yybegin(DECL);
							                        }
								{END}				{int last = codeLevel.size() - 1;
													 if (last == 0){
														codeLevel.set(last, codeLevel.get(last)+1);
													 } else {
														codeLevel.remove(last);
													 }
													}
								{ELSE}				{elseFound = true;
													 int last = codeLevel.size() - 1;
													 codeLevel.set(last, codeLevel.get(last)+1);
													 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> IF (Transition : ELSE \""+yytext()+"\")");
													 yybegin(IF);}
								{CASE}				{int last = codeLevel.size() - 1;
													 codeLevel.set(last, codeLevel.get(last)+1);}
								{DO}				{codeLevel.add(1);}
								{IF}				{if (yytext().toLowerCase().contains("select")){
														codeLevel.add(1);
													 }
													 yybegin(IF);}
								{VAR}				{variable = yytext(); 
													 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> VAR_EQ (Transition : VAR \""+yytext()+"\")");
													 yybegin(VAR_EQ);}
								{SPACE}				{}
	<DECL,EQUAL,NAMING,IF>		.   	           	{}	
		}
		
/******************/
/* NEW LINE STATE */
/******************/
<NEW_LINE>	
		{
			{COMMENT_WORD}		{
                        			LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\")");
                        			yybegin(COMMENT);
                    			}
			{STRING}			{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : STRING \""+yytext()+"\")");
                    			     yybegin(YYINITIAL);
                    			}
			{FALSE}				{variable = yytext();
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> VAR_EQ (Transition : FALSE \""+yytext()+"\")");
								 yybegin(VAR_EQ);}
			{TYPE}        		{location = yytext(); 
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\")");
								 yybegin(NAMING);}
			{IMPLICIT}			{
			                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : IMPLICIT \""+yytext()+"\")");
			                     yybegin(YYINITIAL);
			                    }
			{DECL}				{
                    			     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECL (Transition : DECL \""+yytext()+"\")");
                    			     yybegin(DECL);
                    			}
			{END}				{int last = codeLevel.size() - 1;
								 if (last == 0){
									codeLevel.set(last, codeLevel.get(last)+1);
								 } else {
									codeLevel.remove(last);
								 }
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : END \""+yytext()+"\")");
								 yybegin(YYINITIAL);
								}
			{ELSE}				{elseFound = true;
								 int last = codeLevel.size() - 1;
								 codeLevel.set(last, codeLevel.get(last)+1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IF (Transition : ELSE \""+yytext()+"\")");
								 yybegin(IF);}
			{CASE}				{int last = codeLevel.size() - 1;
								 codeLevel.set(last, codeLevel.get(last)+1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : CASE \""+yytext()+"\")");
								 yybegin(YYINITIAL);}
			{DO}				{codeLevel.add(1);
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : DO \""+yytext()+"\")");
								 yybegin(YYINITIAL);}
			{IF}				{if (yytext().toLowerCase().contains("select")){
									codeLevel.add(1);
								 }
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IF (Transition : IF \""+yytext()+"\")");
								 yybegin(IF);}
			{VAR}				{variable = yytext(); 
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> VAR_EQ (Transition : VAR \""+yytext()+"\")");
								 yybegin(VAR_EQ);}
			{SPACE}				{}
			(\n|\r)+			{}
			.					{
			                         LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> YYINITIAL (Transition : . )");
			                         yybegin(YYINITIAL);
		                         }
		}	
								
/************************/
/* ERROR STATE	        */
/************************/
				[^]            	{
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }