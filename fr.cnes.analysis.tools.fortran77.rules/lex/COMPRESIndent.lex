/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.INST.Indent rule.	    */
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;


import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESIndent
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, NEW_LINE, LINE, AVOID, IF_STATE, TYPE_STATE, DO_STATE

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \! [^\n]*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} |
			   {DATA_TYPE} [\ ]+ {FUNC}| {REC}[\ ]+{SUB}
DATA		 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision  | "TYPE" ( \( [^\)]* \) )?
DATA_TYPE 	 = {DATA} ( \*{INT} )?
INSTRUCT	 = "DO"      | "IF"      | "WHILE"	  | 
			   "WHERE"   | "SELECT"  | "TYPE"
INST		 = {INSTRUCT}[\ ] | {INSTRUCT}\( | {INSTRUCT}\n | {INSTRUCT}\r
ELSE		 = "ELSE"    | "ELSE"[\ ]*"IF"
END			 = "END"
CONTINUE	 = "CONTINUE"
END_INST	 = {END}[\ ]*{INSTRUCT}	| {END}[\ ]&&!({END}[\ ]*\=) 
END_TYPE	 = {END}[\ ]*{TYPE}		| {END}[\ ]*\n	| {END}[\ ]*\r
INCLUDE		 = "INCLUDE "
REC			 = "RECURSIVE"
THEN		 = "THEN"
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
INT			 = [0-9]+
SPACE		 = [\ \r\t\f]
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]
																
%{
    private static final Logger LOGGER = Logger.getLogger(COMPRESIndent.class.getName());
	String location = "MAIN PROGRAM";
	
	int currentCol = 0;
	List<Integer> theoricCol = new LinkedList<Integer>();
	List<Integer> numbers    = new LinkedList<Integer>();
	boolean endLine = true;
	int par = 0;
	int lastNum = 0;
    String parsedFileName;
	
	
	public COMPRESIndent(){
		theoricCol.add(0);
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
return getCheckResults();
%eofval}


%%          

/************************/

				{FREE_COMMENT}	{}

/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{currentCol=0; 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);} 
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{VAR}			{ currentCol+=yytext().length(); 
								 location = location + " " + yytext();
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
								 yybegin(COMMENT);}
<NAMING>    	\n             	{currentCol = 0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<NAMING>		{SPACE}			{currentCol+=yytext().length(); }
<NAMING>    	.              	{currentCol+=yytext().length();}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{theoricCol.add(0); location = yytext(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<YYINITIAL> 	\n             	{currentCol=0; currentCol+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{currentCol+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{currentCol+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);}
<NEW_LINE>		{INT}			{currentCol+=yytext().length(); lastNum = Integer.parseInt(yytext()); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : INT \""+yytext()+"\" )");
                                    yybegin(LINE);}
<NEW_LINE>		{TYPE}        	{theoricCol.add(0); numbers.clear(); location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<NEW_LINE>		{INCLUDE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : INCLUDE \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<NEW_LINE>		{DATA_TYPE}		{
                                    if((theoricCol.size() > 0) && (currentCol<=theoricCol.get(theoricCol.size()-1))){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{INST}			{
                                    if((theoricCol.size() > 0) && (currentCol<=theoricCol.get(theoricCol.size()-1))){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    if(yytext().toLowerCase().contains("if")) { 
                                        if(yytext().contains("(")){
                                            par=1;
                                        }
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> IF_STATE (Transition : INST \""+yytext()+"\" )");
                                        yybegin(IF_STATE);
                                    } else if(yytext().toLowerCase().contains("type")) {
                                        if(yytext().contains("(")){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : INST \""+yytext()+"\" )");
                                            yybegin(AVOID);
                                        } else {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> TYPE_STATE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(TYPE_STATE);
                                        }
                                    } else if(yytext().toLowerCase().contains("do")) {
                                        theoricCol.add(currentCol); 
                                        if(yytext().contains("\n")) {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NEW_LINE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(NEW_LINE);
                                        }
                                     	yybegin(DO_STATE);
                                    } else { 
                                        theoricCol.add(currentCol); 
                                        if(yytext().contains("\n")) {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NEW_LINE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(NEW_LINE);
                                        } else {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : INST \""+yytext()+"\" )");
                                            yybegin(COMMENT);
                                        }
                                    }
                                }
<NEW_LINE>		{ELSE}			{
                                    if((theoricCol.size() > 0) && (currentCol != theoricCol.get(theoricCol.size()-1))) {
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : ELSE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{END_INST}		{
                                    if((theoricCol.size() > 0) && theoricCol.get(theoricCol.size()-1) !=0 && currentCol!=theoricCol.get(theoricCol.size()-1)) {
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    theoricCol.remove(theoricCol.size()-1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : END_INST \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{END_TYPE}		{
                                    theoricCol.remove(theoricCol.size()-1);
                                    if(yytext().contains("\n")){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NEW_LINE (Transition : END_TYPE \""+yytext()+"\" )");
                                        yybegin(NEW_LINE);
                                    } else {
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : END_TYPE \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    }
                                }
<NEW_LINE>		{CONTINUE}		{
                                    if(numbers.contains(lastNum)) {
                                        numbers.remove(numbers.indexOf(lastNum));
                                        theoricCol.remove(theoricCol.size()-1);
                                    }
                                    if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : CONTINUE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		#				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : # )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{currentCol=0;}
<NEW_LINE>		{SPACE}			{currentCol+=yytext().length();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : SPACE \""+yytext()+"\" )");
                                    yybegin(LINE);}
<NEW_LINE>  	.              	{
                                    if((theoricCol.size() > 0) && currentCol!=5 && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : . )");
                                    yybegin(AVOID);
                                }


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{currentCol+=yytext().length();}
<LINE>			{INT}			{currentCol+=yytext().length(); lastNum = Integer.parseInt(yytext());}
<LINE>			{TYPE}        	{theoricCol.add(0); numbers.clear(); location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);}
<LINE>			{INCLUDE}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : INCLUDE \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<LINE>			{DATA_TYPE}		{
                                    if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : DATA_TYPE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{INST}			{
                                    if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    } 
                                    if(yytext().toLowerCase().contains("if")) {
                                        if(yytext().contains("(")){
                                            par=1;
                                        }
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> IF_STATE (Transition : INST \""+yytext()+"\" )");
                                        yybegin(IF_STATE);
                                    } else if(yytext().toLowerCase().contains("type")) {
                                        if(yytext().contains("(")){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : INST \""+yytext()+"\" )");
                                            yybegin(AVOID);
                                        } else {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> TYPE_STATE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(TYPE_STATE);
                                        }
                                    } else if(yytext().toLowerCase().contains("do")) {
                                        theoricCol.add(currentCol); 
                                        if(yytext().contains("\n")){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(NEW_LINE);
                                        }
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DO_STATE (Transition : INST \""+yytext()+"\" )");
                                        yybegin(DO_STATE);
                                    } else { 
                                        theoricCol.add(currentCol); 
                                        if(yytext().contains("\n")){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : INST \""+yytext()+"\" )");
                                            yybegin(NEW_LINE);
                                        } else {
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : INST \""+yytext()+"\" )");
                                            yybegin(COMMENT);
                                        }
                                    }
                                }
<LINE>			{ELSE}			{
                                    if((theoricCol.size() > 0) && currentCol!=theoricCol.get(theoricCol.size()-1)) {
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : ELSE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{END_INST}		{
                                    if((theoricCol.size() > 0) && theoricCol.get(theoricCol.size()-1) !=0 && currentCol!=theoricCol.get(theoricCol.size()-1)) {
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    theoricCol.remove(theoricCol.size()-1);
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : END_INST \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{END_TYPE}		{
                                    theoricCol.remove(theoricCol.size()-1);
                                    if(yytext().contains("\n")){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : END_TYPE \""+yytext()+"\" )");
                                        yybegin(NEW_LINE);
                                    } else {
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : END_TYPE \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    }
                                }
<LINE>			{CONTINUE}		{
                                    if(numbers.contains(lastNum)) {
                                        numbers.remove(numbers.indexOf(lastNum));
                                        theoricCol.remove(theoricCol.size()-1);
                                    }
                                    if((theoricCol.size() > 0) && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location,"The code is not indented.", yyline+1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : CONTINUE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			&				{
                                    if(currentCol==6){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : & )");
                                        yybegin(AVOID);
                                    }
                                }
<LINE>			#				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : # )");
                                    yybegin(COMMENT);}
<LINE>			{VAR}			{}
<LINE>      	\n             	{currentCol=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<LINE>			{SPACE}			{currentCol+=yytext().length();}
<LINE>      	.              	{
                                    if((theoricCol.size() > 0) && currentCol!=5 && currentCol<=theoricCol.get(theoricCol.size()-1)){
                                        LOGGER.fine("Setting error line "+(yyline+1)+" because the code is not indented.");
                                        setError(location ,"The code is not indented.", yyline+1); 
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : . )");
                                    yybegin(AVOID);
                                }
                                 
/************************/
/* AVOID STATE    	    */
/************************/
<AVOID>			{STRING}		{}
<AVOID>			{TYPE}        	{
                                    theoricCol.add(0);
                                    numbers.clear();
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<AVOID>			{INST}			{
                                    if(yytext().toLowerCase().contains("if")) {
                                        if(yytext().contains("(")){
                                            par=1;
                                        }
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> IF_STATE (Transition : INST \""+yytext()+"\" )");
                                        yybegin(IF_STATE);
                                    } else { 
                                        theoricCol.add(currentCol);
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> COMMENT (Transition : INST \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    }
                                }
<AVOID>			{ELSE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> COMMENT (Transition : ELSE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<AVOID>			{END_TYPE}		{
                                    if(theoricCol.size() > 0){
                                        theoricCol.remove(theoricCol.size()-1);
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> COMMENT (Transition : END_TYPE \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<AVOID>			{END_INST}		{
                                    if(theoricCol.size() > 0){
                                        theoricCol.remove(theoricCol.size()-1);
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> COMMENT (Transition : END_INST \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    }
                                }
<AVOID>			{VAR}			{}
<AVOID>			\n				{
                                    currentCol=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<AVOID>			.				{}


/************************/
/* IF_STATE    		    */
/************************/
<IF_STATE>		\(					{par++;}
<IF_STATE>		\)					{par--;}
<IF_STATE>		{THEN}				{theoricCol.add(currentCol);
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IF_STATE -> COMMENT (Transition : THEN \""+yytext()+"\" )");
                                        yybegin(COMMENT);}
<IF_STATE>		\n{SPACE}*{SIMBOL}	{}
<IF_STATE>		\n					{
                                        if(par==0) {
                                            currentCol=0;
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - IF_STATE -> NEW_LINE (Transition : \\n )");
                                            yybegin(NEW_LINE);
                                        }
                                    }
<IF_STATE>		.					{}


/************************/
/* TYPE_STATE  		    */
/************************/
<TYPE_STATE>	\(				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - TYPE_STATE -> COMMENT (Transition : [(] )");
                                    yybegin(COMMENT);
                                }
<TYPE_STATE>	{VAR}			{}
<TYPE_STATE>	\n				{theoricCol.add(currentCol); currentCol=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - TYPE_STATE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<TYPE_STATE>	.				{}


/************************/
/* DO_STATE  		    */
/************************/
<DO_STATE>		{INT}			{numbers.add(Integer.parseInt(yytext())); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_STATE -> COMMENT (Transition : INT \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<DO_STATE>		{VAR}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_STATE -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);}
<DO_STATE>		\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DO_STATE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);}
<DO_STATE>		.				{}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                               }