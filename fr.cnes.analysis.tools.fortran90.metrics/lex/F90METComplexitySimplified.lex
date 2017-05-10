/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the NBCiclomatic.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class F90METComplexitySimplified
%extends AbstractMetric
%public
%ignorecase

%line

%function run
%yylexthrow JFlexException
%type FileValue

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
 * La complexit� cyclomatique d'une m�thode est d�finie par le nombre de chemins 
 * lin�airement ind�pendants qu'il est possible d'emprunter dans cette m�thode.
 * Plus simplement, il s'agit du nombre de pofloats de d�cision de la m�thode (if, case, while, ...)
 * + 1 (le chemin principal).
 * La complexit� cyclomatique d'une m�thode vaut au minimum 1, puisqu'il y a toujours au moins un chemin.
 *
 */
%{
	String location = "MAIN PROGRAM";
	FileValue fileValue;
	float numCiclomatic = 0;
	float numCiclomaticTotal = 0;
	int functionLine = 0;
	
	public F90METComplexitySimplified() {
    }
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
        fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void endLocation() {
		final List<FunctionValue> list =
                this.fileValue.getFunctionValues();
        if (list.isEmpty()) {
            list.add(new FunctionValue(this.location, numCiclomatic+1, functionLine+1));
        } else {
            final FunctionValue last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numCiclomatic+1);
            } else {
				list.add(new FunctionValue(this.location, numCiclomatic+1, functionLine+1));
			}
        }
	}
	
%}

%eofval{
    fileValue.setValue(Float.NaN);
	return fileValue;
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


				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );}
