/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the NBCiclomatic.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.icode.fortran90.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class F90METComplexitySimplified
%extends AbstractChecker
%public
%ignorecase
%column
%line


%function run
%yylexthrow JFlexException
%type List<CheckResult>

%state COMMENT, NAMING, NEW_LINE, LINE

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \!
FUNC         = "FUNCTION "
PROC         = "PROCEDURE "
SUB          = "SUBROUTINE "
PROG         = "PROGRAM "
MOD          = "MODULE "
INTER		 = "INTERFACE "
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | {INTER}
WHILE		 = DO [\ ]+ WHILE		   | do [\ ]+ while	|
			   WHILE [\ ]* \( [^\)]* \) [\ ]* DO	    |
			   while [\ ]* \( [^\)]* \) [\ ]* do
CICLO		 = DO		  | do         | IF    		| if  		|
			   ELSE[\ ]*IF|else[\ ]*if | WHILE      | while     |
			   WHERE	  | where      | ELSEWHERE  | elsewhere |
			   SELECT	  | select
CLOSING		 = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do |
			   END[\ ]*WHERE | end[\ ]*where | END[\ ]*SELECT 	 | 
			   end[\ ]*select 
END			 = END		  | end
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/*
 * La complexité cyclomatique d'une méthode est définie par le nombre de chemins
 * linéairement indépendants qu'il est possible d'emprunter dans cette méthode.
 * Plus simplement, il s'agit du nombre de pofloats de décision de la méthode (if, case, while, ...)
 * + 1 (le chemin principal).
 * La complexité cyclomatique d'une méthode vaut au minimum 1, puisqu'il y a toujours au moins un chemin.
 *
 */
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	float numCiclomatic = 0;
	float numCiclomaticTotal = 0;
	int functionLine = 0;
	
	public F90METComplexitySimplified() {
    }
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	
	private void endLocation() throws JFlexException {
		final List<CheckResult> list = this.getCheckResults();
        if (list.isEmpty()) {
            this.computeMetric(this.location, numCiclomatic + 1, functionLine + 1);
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numCiclomatic+1);
            } else {
				this.computeMetric(this.location, numCiclomatic + 1, functionLine + 1);
			}
        }
	}
	
%}

%eofval{
	this.computeMetric(null, Float.NaN, 0);
	return getCheckResults();
%eofval}
%%

/* This is the general automaton. Each part will be described later. */

				{COMMENT_WORD}	{yybegin(COMMENT);}
				
				
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}


<NAMING>		{VAR}			{numCiclomatic=0; location = location + " " + yytext(); yybegin(COMMENT);}							 
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{STRING}		{yybegin(LINE);}
<YYINITIAL>		{TYPE}        	{location = yytext();functionLine=yyline; yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext();functionLine=yyline; yybegin(NAMING);}
<NEW_LINE>		{WHILE}			{numCiclomatic++; numCiclomaticTotal++;}
<NEW_LINE>		{CICLO}			{numCiclomatic++; numCiclomaticTotal++;}
<NEW_LINE>		{CLOSING}		{}
<NEW_LINE>		{END}			{endLocation();}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext();functionLine=yyline;yybegin(NAMING);}
<LINE>			{WHILE}			{numCiclomatic++; numCiclomaticTotal++;}
<LINE>			{CICLO}			{numCiclomatic++; numCiclomaticTotal++;}
<LINE>			{CLOSING}		{}
<LINE>			{END}			{endLocation();}
<LINE>			{VAR}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{}


				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}