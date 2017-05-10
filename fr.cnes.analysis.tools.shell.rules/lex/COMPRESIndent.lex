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

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESIndent
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\f]
TAB			 = [\t]
SPACETAB	 = {SPACE}|{TAB}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
CONTINUEDLINE = \\ {SPACETAB}* \n

BEGIN		 = "if"		| "case"	| "for"		| "while"	|
			   "until"
CONT		 = "do"		| "then"
END			 = "done"	| "fi"		| "esac"
ELSE		 = "else" 	| "elif"	

IGNORETEXT	 = "<<" {SPACE}* "EOF" [^"<<"]* "EOF" | ` [^`]* `


																
%{
	String location = "MAIN PROGRAM";
	int currentPos = 0, pos = 0; 
	List<Integer> desiredPos = new ArrayList<Integer>();
	/* inTabs is true while parsing tabs at the beginning of a line */
	Boolean inTabs = false;
	/* indentationRequired is true when the next line should be indented compared to the last */
	Boolean indentationRequired = false;
	/* avoid is true when the {} should not be taken into account */
	Boolean avoid = false;
	/* checkEnd is true when in AVOID we are after a ";", requiring that ENDs should be taken into account */
	Boolean checkEnd=false;

    public COMPRESIndent() {
    	desiredPos.add(0);
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
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
						setError(location,"This line is not indented in comparison with the last one.", yyline+1);
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
				setError(location,"This line is not indented in comparison with the last one.", yyline+1);
		}
	}
	
			
%}

%eofval{
	return getViolations();
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
				{VAR}			{location = yytext(); yybegin(AVOID);}
				\n             	{currentPos=0; yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
		
/************************/
/* AVOID STATE	    */
/************************/
/* Consume characters till end of line */
<AVOID>
		{
				;				{checkEnd=true;}
			    {COMMENT_WORD} 	{checkEnd=false; yybegin(COMMENT);}
				{END}			{if(checkEnd && desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);}}
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
				{FUNCTION}     	{inTabs=false; checkIndentation(); indentationRequired=true; desiredPos.add(currentPos+1); yybegin(NAMING);}
				{FUNCT}			{inTabs=false; location = yytext().substring(0,yytext().length()-2).trim(); checkIndentation(); indentationRequired=true; desiredPos.add(currentPos+1); yybegin(AVOID);}
			    {BEGIN}			{inTabs=false; checkIndentation(); indentationRequired=true; desiredPos.add(currentPos+1); yybegin(AVOID);}
			    {CONT} | {ELSE}	{inTabs=false; checkIndentationElse(); indentationRequired=true; yybegin(AVOID);}
			    {END}			{inTabs=false; if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);} indentationRequired=false; checkIndentation(); yybegin(AVOID);}
				{TAB}			{pos=currentPos; currentPos+=yytext().length(); 
								 if(pos==0 | inTabs==true) {
									 /* a tab was found at the beginning of a line */
									 inTabs=true; 
									 setError(location,"Tabulations are not allowed.", yyline+1);
								}}
			    {SPACE}			{if(currentPos==0) {inTabs=true;}; currentPos+=yytext().length();}
				{VAR}			{inTabs=false; checkIndentation(); yybegin(AVOID);}
				{IGNORETEXT}	{}
			    \{				{inTabs=false; yybegin(AVOID);}
			    \}				{inTabs=false; if(desiredPos.size()>=1) {desiredPos.remove(desiredPos.size()-1);} checkIndentationNoChange(); yybegin(AVOID);}
			    \n				{inTabs=false; currentPos=0;}
	      		.              	{inTabs=false; checkIndentation(); yybegin(AVOID);}
		}
			

/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}