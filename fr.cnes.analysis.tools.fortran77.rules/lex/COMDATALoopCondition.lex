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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

%%

%class COMDATALoopCondition
%extends AbstractChecker
%public
%line
%ignorecase
%column

%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* A state called ENTER_DO is created. It allows to determine the condition	 */
/* of a DO-loop and certify that it's not a WHILE-loop.						 */
/* A state called VAR is also defined, where we can see if a variable is 	 */
/* modified.																 */
/* A state called IFLINE is to determine whether the statement is stored. If */
/* it is followed by a THEN, we store it, else it's ignored.				 */
%state COMMENT, NAMING, NEW_LINE, LINE, ENTER_DO, ENTER_WHILE, VAR, PAR

FREE_COMMENT = \!
COMMENT_WORD = \! | "c" | "*"
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
    private static final Logger LOGGER = Logger.getLogger(COMDATALoopCondition.class.getName());

	String location = "MAIN PROGRAM";
	
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
	String parsedFileName;
	
	public COMDATALoopCondition() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
	}
	
	/** If the last identifier is:
	    - DO -> check conditionsDo list
	    - WHILE -> check conditionsWhile list
	**/
	private void closeCondition() throws JFlexException {
        LOGGER.finest("begin method closeCondition");
		int idLength = identifiers.size() - 1;
		if(idLength >= 0){
			if (identifiers.get(idLength).equals("DO")) 
				closeDoLoop();
			else if (identifiers.get(idLength).equals("WHILE"))
				closeWhileLoop();
			identifiers.remove(idLength);
		}else{
            String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
            final String errorMessage = "Analysis failure : Loop's identifier unreachable.";
            throw new JFlexException(this.getClass().getName(), parsedFileName,
                            errorMessage, parsedWord, yyline, yycolumn);
		}
        LOGGER.finest("end method closeCondition");
	}
	
	/** Delete the identifiers and variables in do loop **/
	private void closeDoLoop() {
        LOGGER.finest("begin method closeDoLoop");
		if(!numDo.isEmpty()){
			int condLength = numDo.get(numDo.size()-1);
			for (int i = 0; i < condLength; i++) {
				if(!conditionsDo.isEmpty()) {
					conditionsDo.remove(conditionsDo.size()-1);
				}
			}
			numDo.remove(numDo.size()-1);
		}
        LOGGER.finest("end method closeDoLoop");
	}
	
	/** Delete the identifiers and variables in do while loop **/
	private void closeWhileLoop() {
        LOGGER.finest("begin method closeWhileLoop");
		if(!numWhile.isEmpty()) {
			int condLength = numWhile.get(numWhile.size()-1);
			for (int i = 0; i < condLength; i++) {
				if(!conditionsWhile.isEmpty()) {
					conditionsWhile.remove(conditionsWhile.size()-1);
				}
			}
			numWhile.remove(numWhile.size()-1);
		}
        LOGGER.finest("end method closeWhileLoop");
	}
	
	/** If the last variable is in the list of DO variables -> error **/
	private void checkDo() {
        LOGGER.finest("begin method checkDo");
		if(conditionsDo.contains(descr)) {
			try {
				LOGGER.fine("Setting error line "+yyline+1+" for the variable "+descr+".");
				this.setError(location,"The variable " + descr + " is modified inside the loop.", yyline+1);
			} catch (JFlexException e) {
				e.printStackTrace();
			}
		}
        LOGGER.finest("end method checkDo");
	}
	
	/** If the variables of do while is the to be modified -> error
	    else -> delete variable from list **/
	private void checkDoWhile() {
        LOGGER.finest("begin method checkDoWhile");
		if(!numWhile.isEmpty()) {
			int val = numWhile.get(numWhile.size()-1);
			if(val != 1) {
				numWhile.set(numWhile.size()-1, val-1);
				conditionsWhile.remove(descr);
			}
			else {
				try {
					LOGGER.fine("Setting error line "+yyline+1+" for the variable "+descr+".");
					this.setError(location,"The variable " + descr + " is modified inside the loop.", yyline+1);
				} catch (JFlexException e) {
					e.printStackTrace();
				}
			}
		}
        LOGGER.finest("end method checkDoWhile");
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
				{FREE_COMMENT}	{
                				    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\" )");
                				    yybegin(COMMENT);
                				}

<COMMENT>   	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }  
<COMMENT>   	.              	{}

<NAMING>		{VAR}			{
                                    location = location + " " + yytext();
                                    conditionsDo.clear();
                                    conditionsWhile.clear();
                                    identifiers.clear();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> COMMENT (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NAMING>    	\n             	{conditionsDo.clear(); conditionsWhile.clear(); identifiers.clear();
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n )");
								 yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/* If a WHILE appears, we change the name of last identifier. Otherwise, the */
/* end condition is stored.													 */
<ENTER_DO>		{STRING}		{}												
<ENTER_DO>		{WHILE}			{
                                    identifiers.set(identifiers.size()-1,"WHILE");
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENTER_DO -> ENTER_WHILE (Transition : WHILE \""+yytext()+"\" )");
                                    yybegin(ENTER_WHILE);
                                }
<ENTER_DO>		{VAR}			{conditionsDo.add(yytext()); valueLoop++; }
<ENTER_DO>		\(				{
                                    par=1;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENTER_DO -> PAR (Transition : [(] )");
                                    yybegin(PAR);
                                }
<ENTER_DO>		.				{}
<ENTER_DO>		\n				{
                                    numDo.add(valueLoop);
                                    valueLoop=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENTER_DO -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }

<ENTER_WHILE>	{STRING}		{}
<ENTER_WHILE>	{VAR}			{conditionsWhile.add(yytext()); valueLoop++;}
<ENTER_WHILE>	.				{}
<ENTER_WHILE>	\n				{
                                    numWhile.add(valueLoop);
                                    valueLoop=0;
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ENTER_WHILE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }

<PAR>			\(				{par++;}
<PAR>			\)				{
                                    par--;
                                    if(par==0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PAR -> ENTER_DO (Transition : [)] )");
                                        yybegin(ENTER_DO);
                                    }
                                }
<PAR>			[^]			{}

/* If a VAR type appears, we check if it's followed by an equal sign, which	*/
/* means it's been modified. In that case, an error is set.					*/
<VAR>			{STRING}		{}
<VAR>			\=				{
                                     if(!identifiers.isEmpty()) {
    								 	if(identifiers.get(identifiers.size()-1).equals("DO")){
    								 	  checkDo();
    								 	}
    								 	if(identifiers.get(identifiers.size()-1).equals("WHILE")){
    								 	  checkDoWhile();
    								 	}
    								 }
    								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR -> LINE (Transition : [=] )");
    								 yybegin(LINE);
								}
<VAR>			\=\=			{}
<VAR>			[\ ]+			{}
<VAR>			.				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }
<VAR>			\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }

<YYINITIAL>		{COMMENT_WORD}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>  	{TYPE}         	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<YYINITIAL>		{RULE_WORD}		{
                                    identifiers.add("DO");
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> ENTER_DO (Transition : RULE_WORD \""+yytext()+"\" )");
                                    yybegin(ENTER_DO);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }

/* If END is found, we check whether it corresponds to a DO-loop. If it is	*/
/* we remove the last condition word. Then, we remove last indentifier.		*/
<NEW_LINE>		{COMMENT_WORD}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{COMMENT_WORD}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>  	{TYPE}         	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{RULE_WORD}		{
                                    identifiers.add("DO");
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> ENTER_DO (Transition : RULE_WORD \""+yytext()+"\" )");
    								yybegin(ENTER_DO);
								}
<NEW_LINE>		{END}			{
                                    if (!identifiers.isEmpty()){
                                        closeCondition();
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : END \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{VAR}			{condition = yytext();
								 if (!conditionsDo.isEmpty()) {
								 	if (conditionsDo.contains(condition)) {
								 		descr = yytext();
								 		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> VAR (Transition : VAR \""+yytext()+"\" )");
								 		yybegin(VAR);
								 	}
								 }
								 if (!conditionsWhile.isEmpty()) {
								 	if (conditionsWhile.contains(condition)) {
								 		descr = yytext();
								 		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> VAR (Transition : VAR \""+yytext()+"\" )");
								 		yybegin(VAR);
								 	}
								 }
								}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }

<LINE>			{STRING}		{}
<LINE>		  	{TYPE}         	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>			{RULE_WORD}		{
                                    identifiers.add("DO");
    								LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> ENTER_DO (Transition : RULE_WORD \""+yytext()+"\" )");
    								yybegin(ENTER_DO);
								}
<LINE>			{END}			{
                                    if (!identifiers.isEmpty()){
                                        closeCondition();
                                    }
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : END \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{VAR}			{condition = yytext();
								 if (!conditionsDo.isEmpty()) {
								 	if (conditionsDo.contains(condition)) {
								 		descr = yytext();
								 		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> VAR (Transition : VAR \""+yytext()+"\" )");
								 		yybegin(VAR);
								 	}
								 }
								 if (!conditionsWhile.isEmpty()) {
								 	if (conditionsWhile.contains(condition)) {
								 		descr = yytext();
								 		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> VAR (Transition : VAR \""+yytext()+"\" )");
								 		yybegin(VAR);
								 	}
								 }
								}
<LINE>      	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}

				[^]            {
                                    String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                                }
