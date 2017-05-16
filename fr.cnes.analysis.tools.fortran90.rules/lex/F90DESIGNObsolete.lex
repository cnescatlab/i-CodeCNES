/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/*****************************************************************************/
/* This file is used to generate a rule checker for F90.ARCH.Obsolete rule	 */
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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90DESIGNObsolete
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>

%state COMMENT, NAMING, NEW_LINE, LINE, HOLL, ASSIGN, GOTO

COMMENT_WORD = "!"
TYPE		 = "function"  | "procedure" | "subroutine"  | "program" | "module" |"interface"
FALSE        = [a-zA-Z0-9\_]({TYPE}) | ({TYPE})[a-zA-Z0-9\_] | [a-zA-Z0-9\_]({TYPE})[a-zA-Z0-9\_]
			   | [^a-zA-Z0-9\_]("module")({SPACE}*)("procedure")[^a-zA-Z0-9\_]

SPACE        = [\ \t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/* Word used to recognize PAUSE. */
PAUSE		 = [^a-zA-Z0-9\_]("pause")[^a-zA-Z0-9\_]
/* Word used to recognize GOTO. */
GOTO		 = [^a-zA-Z0-9\_]("go"){SPACE}*("to")[^a-zA-Z0-9\_]
/* Word used to recognize FORMAT1. */
FORMAT		 = [^a-zA-Z0-9\_]("format"){SPACE}*"("
ASSIGN		 = "assign"
/* Word used to recognize hollerith declaration. */
HOLLERITH	 = [1-9]+[0-9]*("h")[a-zA-Z\ \t\_]+
/* Word used to recognize alternate return declaration. */
RETURN		 = [^a-zA-Z0-9\_]("return"){SPACE}*[1-9]+[0-9]*
/* Word used to recognize end if branch. */
IF_BRANCH    = [1-9]+[0-9]*{SPACE}*("end"){SPACE}*("if")
/* Character errror */
CHAR		 = "character" {SPACE}* \*

%{
	String location = "MAIN PROGRAM"; 
	/** Rule used to assert that arithmetical if is not used. **/
	AbstractRule rule1 = new F90DESIGNObsoleteArithmeticalIf();
	/** Rule used to assert that do loop ending branch is done on a continue or an end do. **/
	AbstractRule rule2 = new F90DESIGNObsoleteDoEnding();
	/** Rule used to assert that do loop only use integer variables. **/
	AbstractRule rule3 = new F90DESIGNObsoleteDoReal();
	/** Rule used to assert that two do loops do not share the same label. **/
	AbstractRule rule4 = new F90DESIGNObsoleteDoShared();
	
	List<String> formatsNum = new LinkedList<String>();
	
	public F90DESIGNObsolete() {
    }
	
	@Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
		
		/** Initializing first rule. **/
        this.rule1.setContribution(this.getContribution());
        this.rule1.setInputFile(file);
		
		/** Initializing second rule. **/
        this.rule2.setContribution(this.getContribution());
        this.rule2.setInputFile(file);
		
		/** Initializing third rule. **/
        this.rule3.setContribution(this.getContribution());
        this.rule3.setInputFile(file);
		
		/** Initializing fourth rule. **/
        this.rule4.setContribution(this.getContribution());
        this.rule4.setInputFile(file);
		
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
    }
	
	/**
	 * Method used to sort violations on increasing order considering their lines.
	 **/
	private void sortResults() {
        Collections.sort(getViolations(), new Comparator<Violation>() {
            @Override
            public int compare(final Violation o1, final Violation o2) {
                return o1.getLine().compareTo(o2.getLine());
            }
        });
    }
%}

%eofval{ 
	/** We had results of 4 other rules. **/
	getViolations().addAll(rule1.run());
	getViolations().addAll(rule2.run());
	getViolations().addAll(rule3.run());
	getViolations().addAll(rule4.run());
	
	/** The results are sorted on lines' increasing order. **/
	sortResults();
	return getViolations(); 
%eofval}


%%          
			{COMMENT_WORD}	{yybegin(COMMENT);}

/************************/
/* COMMENT STATE        */
/************************/
<COMMENT>   	
		{
			\n             	{yybegin(NEW_LINE);}  
			.              	{}
		}

/************************/
/* NAMING STATE        	*/
/************************/
<NAMING>		
		{
			{VAR}			{location = location + " " + yytext(); 
							 yybegin(COMMENT);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}

/************************/
/* YYINITAL STATE       */
/************************/
<YYINITIAL>  	
		{
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}        	{location = yytext(); yybegin(NAMING);}
			\n             	{yybegin(NEW_LINE);}
			.              	{yybegin(LINE);}
		}

/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>  	
		{
			{STRING}		{yybegin(LINE);}
			{FALSE}			{yybegin(LINE);}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{PAUSE}			{setError(location,"The instruction PAUSE is not allowed.", yyline+1); 
							 yybegin(LINE);}
			{GOTO}			{yybegin(GOTO);}
			{FORMAT}		{yybegin(HOLL);}
			{RETURN}		{setError(location,"The alternate return statement is not allowed. ", yyline+1); 
							 yybegin(LINE);}
			{IF_BRANCH}		{setError(location,"There is a branch on an END IF statement. It is not allowed", yyline+1); 
							 yybegin(LINE);}
			{ASSIGN}		{yybegin(ASSIGN);}
			{CHAR}			{setError(location,"The use of CHARACTER* is not allowed.", yyline+1); 
							 yybegin(LINE);}
			[0-9]+			{formatsNum.add(yytext()); yybegin(LINE);}
			{SPACE}			{}
			\n             	{}
			.              	{yybegin(LINE);}
		}

/************************/
/* LINE STATE           */
/************************/
<LINE>		
		{
			{STRING}		{}
			{FALSE}			{}
			{TYPE}         	{location = yytext(); 
							 yybegin(NAMING);}
			{PAUSE}			{setError(location,"The instruction PAUSE is not allowed.", yyline+1);}
			{GOTO}			{yybegin(GOTO);}
			{FORMAT}		{yybegin(HOLL);}
			{RETURN}		{setError(location,"There is a branch on an END IF statement. It is not allowed.", yyline+1);}
			{ASSIGN}		{yybegin(ASSIGN);}
			[0-9]+			{formatsNum.add(yytext()); yybegin(LINE);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* HOLL STATE           */
/************************/
<HOLL>		
		{
			{STRING}		{}
			{FALSE}			{}
			{HOLLERITH}		{setError(location,"The instruction HOLLERITH is not allowed inside FORMAT. Error in " + yytext() + " used.", yyline+1); yybegin(LINE);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* ASSIGN STATE           */
/************************/
<ASSIGN>		
		{
			{STRING}		{}
			{FALSE}			{}
			[0-9]+			{if (formatsNum.contains(yytext()))
							 	setError(location,"The instruction ASSIGN contains the label for the FORMAT instruction.", yyline+1); yybegin(LINE);}
			\n             	{yybegin(NEW_LINE);}
			.              	{}
		}
		
/************************/
/* GOTO STATE           */
/************************/
<GOTO>		
		{
			{STRING}					{}
			{FALSE}						{}
			\( [^\)]+ \) {SPACE}* {VAR}	{setError(location,"The instruction calculed GOTO is not allowed.", yyline+1); yybegin(LINE);}
			[0-9]+						{}
			\n             				{yybegin(NEW_LINE);}
			.              				{}
		}
		
/************************/
/* THROW ERROR          */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}