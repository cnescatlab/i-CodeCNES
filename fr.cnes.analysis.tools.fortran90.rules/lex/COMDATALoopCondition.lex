/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for Tr.ModifCondSortie rule. */
/* For further information on this, we advise you to refer to RNC manuals.	 */
/* As many comments have been done on the ExampleRule.lex file, this file    */
/* will restrain its comments on modifications.								 */
/*																			 */
/*****************************************************************************/

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

%class COMDATALoopCondition
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state called ENTER_DO is created. It allows to determine the condition	 */
/* of a DO-loop and certify that it's not a WHILE-loop.						 */
/* A state called VAR is also defined, where we can see if a variable is 	 */
/* modified.																 */
/* A state called IFLINE is to determine whether the statement is stored. If */
/* it is followed by a THEN, we store it, else it's ignored.				 */
%state COMMENT, NAMING, NEW_LINE, LINE, ENTER_DO, ENTER_WHILE, VAR,PAR

FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
INTERFACE	 = INTERFACE  | interface
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTERFACE}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]* (\% [a-zA-Z][a-zA-Z0-9\_]*)?
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* A list of String is made. It represents all variable that can't be changed.	*/
/* Another list of String is made, with identifiers such as DO and IF. Its aim  */
/* is to determine when an END is met, if depth can be reduce or not. It won't	*/
/* be decreased if the END corresponds to an IF statement.						*/
/* A String name condition is created to store the current word.				*/
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	
	List<String> identifiers = new LinkedList<String>();
	List<String> conditionsDo = new LinkedList<String>();
	List<String> conditionsWhile = new LinkedList<String>();
	List<Integer> numDo = new LinkedList<Integer>();
	List<Integer> numWhile = new LinkedList<Integer>();
	 
	String condition = "";
	int par = 0;
	int valueLoop = 0;
	boolean doVar = true;
	String descr = "";
	
	
	public COMDATALoopCondition() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/** If the last identifier is:
	    - DO -> check conditionsDo list
	    - WHILE -> check conditionsWhile list
	**/
	private void closeCondition() throws JFlexException {
		int idLength = identifiers.size() - 1;
		if(idLength >= 0){
			if (identifiers.get(idLength).equals("DO")) 
				closeDoLoop();
			else if (identifiers.get(idLength).equals("WHILE"))
				closeWhileLoop();
			identifiers.remove(idLength);
		}else{
			
            final String errorMessage = "Analysis failure : Identifier unreachable.";
            throw new JFlexException(this.getClass().getName(), parsedFileName,
                            errorMessage, yytext(), yyline, yycolumn);
		}
	}
	
	/** Delete the identifiers and variables in do loop **/
	private void closeDoLoop() {
		if(!numDo.isEmpty()){
			int condLength = numDo.get(numDo.size()-1);
			for (int i = 0; i < condLength; i++) {
				if(!conditionsDo.isEmpty()) {
					conditionsDo.remove(conditionsDo.size()-1);
				}
			}
			numDo.remove(numDo.size()-1);
		}
	}
	
	/** Delete the identifiers and variables in do while loop **/
	private void closeWhileLoop() {
		if(!numWhile.isEmpty()) {
			int condLength = numWhile.get(numWhile.size()-1);
			for (int i = 0; i < condLength; i++) {
				if(!conditionsWhile.isEmpty()) {
					conditionsWhile.remove(conditionsWhile.size()-1);
				}
			}
			numWhile.remove(numWhile.size()-1);
		}
	}
	
	/** If the last variable is in the list of DO variables -> error **/
	private void checkDo() {
		if(conditionsDo.contains(descr)) {
			try {
				this.setError(location,"The variable " + descr + " is modified inside the loop.", yyline+1);
			} catch (JFlexException e) {
				e.printStackTrace();
			}
		}
	}
	
	/** If the variables of do while is the to be modified -> error
	    else -> delete variable from list **/
	private void checkDoWhile() {
		if(!numWhile.isEmpty()) {
			int val = numWhile.get(numWhile.size()-1);
			if(val != 1) {
				numWhile.set(numWhile.size()-1, val-1);
				conditionsWhile.remove(descr);
			}
			else {
				try {
					this.setError(location,"The variable " + descr + " is modified inside the loop.", yyline+1);
				} catch (JFlexException e) {
					e.printStackTrace();
				}
			}
		}
	}
%}

%eofval{
    return getCheckResults();
%eofval}

/* Transition word is do (or DO). If this word is met, we focus on out condition */
/* end verifies that it's not modified within the DO-loop. If WHILE is found,    */
/* nothing has to be done. We also look for END, which means modification can be */
/* done afterward. IF and ELSE are also included. An expression to define 	 	 */
/* variables syntax is also added.												 */
/* The word THEN is also a part of new words, as it helps to determine if an IF	 */
/* declaration expects an END word or not.										 */
RULE_WORD = "do"
END       = "end"[\ ]*"do"	| 
			"end"[\ ]*"while"	|
			"continue"
WHILE	  = "while"

%%          
				{FREE_COMMENT}	{yybegin(COMMENT);}

<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{location = location + " " + yytext();
								 conditionsDo.clear(); conditionsWhile.clear(); identifiers.clear();
								 yybegin(COMMENT);}
<NAMING>    	\n             	{conditionsDo.clear(); conditionsWhile.clear(); identifiers.clear();
								 yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/* If a WHILE appears, we change the name of last identifier. Otherwise, the */
/* end condition is stored.	
<ENTER_DO>		{STRING}		{}												 */
<ENTER_DO>		{WHILE}			{identifiers.set(identifiers.size()-1,"WHILE"); yybegin(ENTER_WHILE);}
<ENTER_DO>		{VAR}			{conditionsDo.add(yytext()); valueLoop++; }
<ENTER_DO>		\(				{par=1; yybegin(PAR);}
<ENTER_DO>		.				{}
<ENTER_DO>		\n				{numDo.add(valueLoop); valueLoop=0; yybegin(NEW_LINE);}

<ENTER_WHILE>	{STRING}		{}
<ENTER_WHILE>	{VAR}			{conditionsWhile.add(yytext()); valueLoop++;}
<ENTER_WHILE>	.				{}
<ENTER_WHILE>	\n				{numWhile.add(valueLoop); valueLoop=0; yybegin(NEW_LINE);}

<PAR>			\(				{par++;}
<PAR>			\)				{par--; if(par==0) yybegin(ENTER_DO);}
<PAR>			[^]			{}

/* If a VAR type appears, we check if it's followed by an equal sign, which	*/
/* means it's been modified. In that case, an error is set.					*/
<VAR>			{STRING}		{}
<VAR>			\=				{if(!identifiers.isEmpty()) {
								 	if(identifiers.get(identifiers.size()-1).equals("DO")) checkDo();
								 	if(identifiers.get(identifiers.size()-1).equals("WHILE")) checkDoWhile();
								 }
								 yybegin(LINE);
								}
<VAR>			\=\=			{}
<VAR>			[\ ]+			{}
<VAR>			.				{yybegin(LINE);}
<VAR>			\n				{yybegin(NEW_LINE);}

<YYINITIAL>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>		{RULE_WORD}		{identifiers.add("DO");
								 yybegin(ENTER_DO);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/* If END is found, we check whether it corresponds to a DO-loop. If it is	*/
/* we remove the last condition word. Then, we remove last indentifier.		*/
<NEW_LINE>		{STRING}		{}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{RULE_WORD}		{identifiers.add("DO");
								 yybegin(ENTER_DO);}
<NEW_LINE>		{END}			{if (!identifiers.isEmpty()) closeCondition();
								 yybegin(COMMENT);}
<NEW_LINE>		{VAR}			{condition = yytext();
								 if (!conditionsDo.isEmpty()) {
								 	if (conditionsDo.contains(condition)) {
								 		descr = yytext();
								 		yybegin(VAR);
								 	}
								 }
								 if (!conditionsWhile.isEmpty()) {
								 	if (conditionsWhile.contains(condition)) {
								 		descr = yytext();
								 		yybegin(VAR);
								 	}
								 }
								}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}

<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{RULE_WORD}		{identifiers.add("DO");
								 yybegin(ENTER_DO);}
<LINE>			{END}			{if (!identifiers.isEmpty()) closeCondition();
								 yybegin(COMMENT);}
<LINE>			{VAR}			{condition = yytext();
								 if (!conditionsDo.isEmpty()) {
								 	if (conditionsDo.contains(condition)) {
								 		descr = yytext();
								 		yybegin(VAR);
								 	}
								 }
								 if (!conditionsWhile.isEmpty()) {
								 	if (conditionsWhile.contains(condition)) {
								 		descr = yytext();
								 		yybegin(VAR);
								 	}
								 }
								}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}

				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }