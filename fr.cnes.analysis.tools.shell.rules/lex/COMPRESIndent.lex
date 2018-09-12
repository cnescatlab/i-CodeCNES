/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.PRES.Indent rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.EmptyStackException;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.Function;

%%

%class COMPRESIndent
%extends AbstractChecker
%public
%column
%line

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, CONSUME_LINE, BEGINFUNC, STRING_SIMPLE, STRING_DOUBLE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {FNAME}{SPACETAB}*[\(]{SPACETAB}*[\)]
FNAME		 = [a-zA-Z0-9\.\!\-\_\@\?\+]+

SPACE		 = [\ \r\f]
TAB			 = [\t]
SPACETAB	 = {SPACE}|{TAB}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
CONTINUEDLINE = \\ {SPACETAB}* \n

CASE_STMT	 = [^"\n""("" ""\r""\f""#"][^"\n""("]*\)

CASE		 = "case"
ESAC		 = "esac"

BEGIN		 = "if"		| "case"	| "for"		| "while"	| "until"
CONT		 = "do"		| "then"
END			 = "done"	| "fi"		| "esac"
ELSE		 = "else" 	| "elif"	

FUNCSTART		= \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND			= \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

STRING_D		= \"
IGNORE_STRING_D = [\\][\"]
STRING_S	 	= \'
IGNORE_STRING_S = [\\][\']

IGNORETEXT	 = "<<" {SPACE}* "EOF" [^"<<"]* "EOF" | ` [^`]* `


																
%{
	/* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
	
	String location = MAINPROGRAM;
	/* functionLine: the beginning line of the function */
	int functionLine = 0;

    private String parsedFileName;
	int currentPos = 0, pos = 0; 
	List<Integer> desiredPos = new ArrayList<Integer>();
	/* inTabs is true while parsing tabs at the beginning of a line */
	Boolean inTabs = false;
	/* indentationRequired is true when the next line should be indented compared to the last */
	Boolean indentationRequired = false;
	/* checkEnd is true when in CONSUME_LINE we are after a ";", requiring that ENDs should be taken into account */
	Boolean checkEnd=false;
	
	/* firstInCase is true when the case statement is the first one of a case structure */
	Boolean firstInCase = false;
	
	/* decrementAtEnd is true when the body of the function is incremented, eg. when body is an if */
	/* with no { and is incremented compared to the function line */
	Boolean decrementAtEnd = false;

	private Stack<Function> functionStack = new Stack<>();

    public COMPRESIndent() {
    	desiredPos.add(0);
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
        this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	/** 
	  * Check if the position of the current value is less than the expected value
	  * (taking into account indentation rules)
	  */
	private void checkIndentation() throws JFlexException {
		int index = desiredPos.size() - 1;
		if (index >= 0) {
			int value = desiredPos.get(index);
			if (currentPos < value) {
				setError(location,"This line is not indented in comparison with the last one.", yyline+1);
			} else {
				if (currentPos != value) {
					 if (indentationRequired == true) {
						/* we are at the beginning of a new indentation. The first line sets the position */
						desiredPos.remove(index);
						desiredPos.add(currentPos);
						indentationRequired = false; /* continue at same position */ 
					} else { /* indentationRequired == false */
						/* we are not at the beginning of a new indentation. The indentation should be the same as the last line  */
						setError(location,"This line should not be indented in comparison with the last one.", yyline+1);
					}
				} else { /* currentPos == value */
					/* The indentation is what was expected */
					indentationRequired = false;
				}
			}  
		} else {
			if (currentPos != 0) {
				/* we should be at the beginning of the line  */
				setError(location,"This line is not indented in comparison with the last one.", yyline+1); 
			}
		}
	}	
	
	/** 
	  * Check if the position of the current value is less than the expected value
	  * (taking into account indentation rules). The desiredPositin value will not be changed
	  */
	private void checkIndentationNoChange() throws JFlexException {
		int index = desiredPos.size() - 1;
		if (index >= 0) {
			int value = desiredPos.get(index);
			if (currentPos < value) {
				setError(location,"This line is not indented in comparison with the last one.", yyline+1);
			 } 
		}
	}
	
	/** 
	  * The 'else' must be in the same column as its 'if'
	  */
	private void checkIndentationElse() throws JFlexException {
		int index = desiredPos.size() - 2;
		if (index >= 0) {
			int value = desiredPos.get(index);
			if (currentPos != value) 
				setError(location,"This line is not aligned with its corresponding structure.", yyline+1);
		}
	}

	private void endLocation() throws JFlexException {
		try{
		    Function functionFinished = functionStack.pop();
			if (!functionStack.empty()) {
				/* there is a current function: change location to this function */
				location = functionStack.peek().getName();
			} else {
				/* we are in the main program: change location to main */
				location = MAINPROGRAM;
			}
			if (decrementAtEnd) {
				if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
				decrementAtEnd = false;
			}
		}catch(EmptyStackException e){
        		final String errorMessage = e.getMessage();
            	throw new JFlexException(this.getClass().getName(), parsedFileName,
        errorMessage, yytext(), yyline, yycolumn);
		}
	}	
			
%}

%eofval{
	return getCheckResults();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{currentPos=0; yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    	*/
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); yybegin(BEGINFUNC);}
				\n             	{currentPos=0; yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* CONSUME_LINE STATE	    */
/************************/
/* Consume characters till end of line */
<CONSUME_LINE>
		{
				;				{checkEnd=true;}
			    {COMMENT_WORD} 	{checkEnd=false; yybegin(COMMENT);}
				{IGNORE_STRING_D}	{}
 				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{IGNORE_STRING_S}	{}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
				{ESAC}			{
									if(checkEnd && desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}
									firstInCase = false;
								}
				{END}			{
									if(checkEnd && desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}
								}
	      		{FUNCEND}		{ /* for the remaining endings not in END */
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}
		      					}															
				{IGNORETEXT}	{}
				{CONTINUEDLINE}	{}
				\n				{checkEnd=false; currentPos=0; yybegin(YYINITIAL);}
	      		.              	{}
		}	

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{inTabs=false; yybegin(COMMENT);}
 				{IGNORE_STRING_D}	{}
				{STRING_D}		{yybegin(STRING_DOUBLE);}
				{IGNORE_STRING_S}	{}
				{STRING_S}		{yybegin(STRING_SIMPLE);}
				{FUNCTION}     	{
									inTabs=false; 
									checkIndentation();
									indentationRequired=true;
									desiredPos.add(currentPos+1); 
									yybegin(NAMING);
								}
				{FUNCT}			{
									inTabs=false;
									location = yytext().substring(0,yytext().length()-2).trim();
									checkIndentation();
									indentationRequired=true;
									desiredPos.add(currentPos+1);
									yybegin(BEGINFUNC);
								}
				{CASE}			{
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									} 								 
									inTabs=false;
									checkIndentation();
									indentationRequired=true;
									desiredPos.add(currentPos+1);
									/* Next case statement will be the first */
									firstInCase = true;
									yybegin(CONSUME_LINE);
								}
			    {BEGIN}			{
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									} 								 
									inTabs=false;
									checkIndentation();
									indentationRequired=true;
									desiredPos.add(currentPos+1);
									yybegin(CONSUME_LINE);
								}							
			    {CONT} | {ELSE}	{
									inTabs=false;
									checkIndentationElse();
									indentationRequired=true;
									yybegin(CONSUME_LINE);
								}
			    {ESAC}			{
									inTabs=false;
									if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
									/* Another one to compensate for the added indentation of the previous case statement */
									if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
									indentationRequired=false;
									checkIndentation();
									firstInCase = false;
									if(!functionStack.empty()){
										if(functionStack.peek().isFinisher(yytext())){
											if(functionStack.peek().getStarterRepetition()>0) {
												functionStack.peek().removeStarterRepetition();
											} else {
												endLocation();
											}
										}
									}
									yybegin(CONSUME_LINE);
								}
				{END}			{
									inTabs=false;
									if(desiredPos.size()>=1) {
										desiredPos.remove(desiredPos.size()-1);}
									indentationRequired=false;
									checkIndentation();
									if(!functionStack.empty()){
										if(functionStack.peek().isFinisher(yytext())){
											if(functionStack.peek().getStarterRepetition()>0) {
												functionStack.peek().removeStarterRepetition();
											} else {
												endLocation();
											}
										}
									}
									yybegin(CONSUME_LINE);
								}
				{TAB}			{pos=currentPos; currentPos+=yytext().length(); 
								 if(pos==0 | inTabs==true) {
									 /* a tab was found at the beginning of a line */
									 inTabs=true; 
									 setError(location,"Tabulations are not allowed.", yyline+1);
								}}
			    {SPACE}+		{if(currentPos==0) {inTabs=true;}; currentPos+=yytext().length();}
				{VAR}			{inTabs=false; checkIndentation(); yybegin(CONSUME_LINE);}
				{IGNORETEXT}	{}
			    \{				{
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									} 					
									inTabs=false; 
									yybegin(CONSUME_LINE);
								}
			    \}				{
									inTabs=false; 
									if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
									checkIndentationNoChange();
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}								
									yybegin(CONSUME_LINE);
								}
				{FUNCSTART}		{ /* for the remaining beginnings not in BEGIN */
									if(!functionStack.empty()){
										if(functionStack.peek().getFinisher().equals(Function.finisherOf(yytext()))){
											functionStack.peek().addStarterRepetition();
										}
									} 
									yybegin(CONSUME_LINE);
		      					}						
	      		{FUNCEND}		{ /* for the remaining endings not in END */
									if(!functionStack.empty()){
		      							if(functionStack.peek().isFinisher(yytext())){
		      								if(functionStack.peek().getStarterRepetition()>0) {
	      									    functionStack.peek().removeStarterRepetition();
		      								} else {
		      									endLocation();
		      								}
										}
									}
		      					}								
			    \n				{inTabs=false; currentPos=0;}
				{CASE_STMT}		{
									inTabs=false;
									if (!firstInCase) {
										if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}
										checkIndentationNoChange();
									} else {
										firstInCase = false;
										checkIndentation();
									}
									indentationRequired=true;
									desiredPos.add(currentPos+1); 
									yybegin(CONSUME_LINE);
								}
	      		.              	{inTabs=false; checkIndentation(); yybegin(CONSUME_LINE);}
		}

/************************/
/* BEGINFUNC STATE	    */
/************************/
/*
 * This state target is to retrieve the function starter. For more information on fonction starter, have a look on {@link Function} class.
 * Pending this starter, the function ender can be defined.
 *
 */ 
<BEGINFUNC>
		{
				\(\)			{}
				"\n"{SPACE}*	{
									currentPos = currentPos+yytext().length() -1;
									decrementAtEnd = true;
								}
			    {BEGIN}			{
									inTabs=false;
									checkIndentation();
									indentationRequired=true;
									desiredPos.add(currentPos+1);
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
									yybegin(CONSUME_LINE);
								}
			    \{				{
									inTabs=false; 
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
									yybegin(CONSUME_LINE);
								}
				{FUNCSTART}		{
									Function function;
									function = new Function(location, functionLine, yytext());
									functionStack.push(function);
								 	yybegin(CONSUME_LINE);
							 	}
			   	[^]|{SPACE}  {}
		}

/*
 * The string states are designed to avoid problems due to patterns found in strings.
 */ 
/************************/
/* STRING_SIMPLE STATE	    */
/************************/
<STRING_SIMPLE>   	
		{
				{IGNORE_STRING_S}	{}
				{STRING_S}    		{yybegin(CONSUME_LINE);}  
		  	 	[^]|{SPACE}  		{}
		}

/************************/
/* STRING_DOUBLE STATE	    */
/************************/
<STRING_DOUBLE>   	
		{
				{IGNORE_STRING_D}	{}
				{STRING_D}    		{yybegin(CONSUME_LINE);}  
		  	 	[^]|{SPACE}  		{}
		}		

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}