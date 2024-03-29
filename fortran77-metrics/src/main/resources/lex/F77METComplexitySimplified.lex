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

package fr.cnes.icode.fortran77.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.List;

import java.util.logging.Logger;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;



%%

%class F77METComplexitySimplified
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
COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = "FUNCTION "
PROC         = "PROCEDURE "
SUB          = "SUBROUTINE "
PROG         = "PROGRAM "
MOD          = "MODULE "
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
CICLO 		 = DO		  | do         | IF    		| if  		|
			   ELSE[\ ]*IF|else[\ ]*if
CLOSING		 = END[\ ]*IF | end[\ ]*if | END[\ ]*DO | end[\ ]*do
END			 = END		  | end
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"

/*
 * La complexite cyclomatique d'une methode est definie par le nombre de chemins 
 * lineairement independants qu'il est possible d'emprunter dans cette methode.
 * Plus simplement, il s'agit du nombre de pofloats decision de la methode (if, case, while, ...)
 * + 1 (le chemin principal).
 * La complexite cyclomatique d'une methode vaut au minimum 1, puisqu'il y a toujours au moins un chemin.
 *
 */
%{

    private static final Logger LOGGER = Logger.getLogger(F77METComplexitySimplified.class.getName());
    

	String location = "MAIN PROGRAM";
    private String parsedFileName;
	float numCiclomatic = 0;
	float numCiclomaticTotal = 0;
	int functionLine = 0;
	
	
	public F77METComplexitySimplified() {
	
    }
	
	@Override
	public void setInputFile(File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
        this.parsedFileName = file.toString();
        LOGGER.finest("end method setInputFile");       
	}
	
    private void endLocation() throws JFlexException {
        LOGGER.finest("begin method endLocation");
        final List<CheckResult> list = this.getCheckResults();
        if (list.isEmpty()) {
            this.computeMetric(this.location, numCiclomatic + 1, functionLine + 1);
        } else {
            final CheckResult last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numCiclomatic + 1);
            } else {
                this.computeMetric(this.location, numCiclomatic + 1, functionLine + 1);
            }
        }
        LOGGER.finest("end method endLocation");
    }

	
%}

%eofval{
	this.computeMetric(null, Float.NaN, 0);
	return getCheckResults();
%eofval}
%eofclose
%%

/* This is the general automaton. Each part will be described later. */

				{FREE_COMMENT}	{
				                    LOGGER.finest(" [ALL] -> COMMENT (Transition : FREE_COMMENT)");
				                    yybegin(COMMENT);
				                    
				                }
				
				
<COMMENT>   	\n             	{
                                    LOGGER.finest("COMMENT -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<COMMENT>   	.              	{
                                }


<NAMING>		{VAR}			{    
                                    numCiclomatic=0;functionLine=yyline; location = location + " " + yytext(); 
                                    LOGGER.finest("Setting values [numCliclomatic ="+numCiclomatic+" | functionLine = "+ functionLine+" | location = "+location+"]");
                                    LOGGER.finest("NAMING -> COMMENT (Transition : VAR)");
                                    yybegin(COMMENT);
                                }							 
<NAMING>    	\n             	{
                                    LOGGER.finest("NAMING -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.finest("YYINITIAL -> COMMENT (Transition : COMMENT_WORD)");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>		{STRING}		{
                                    LOGGER.finest("YYINITIAL -> LINE (Transition : STRING)");
                                    yybegin(LINE);
                                }
<YYINITIAL>		{TYPE}        	{
                                    location = yytext(); 
                                    LOGGER.finest("Setting value [location="+location);
                                    LOGGER.finest("YYINITIAL -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.finest("YYINITIAL -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.finest("YYINITIAL -> LINE (Transition : .)");
                                    yybegin(LINE);
                                }


<NEW_LINE>  	{COMMENT_WORD} 	{
                                    LOGGER.finest("NEW_LINE -> COMMENT (Transition : COMMENT_WORD)");
                                    yybegin(COMMENT);
                                }
<NEW_LINE>		{STRING}		{
                                    LOGGER.finest("NEW_LINE -> LINE (Transition : \\n)");
                                    yybegin(LINE);
                                }
<NEW_LINE>  	{TYPE}         	{
                                    location = yytext(); 
                                    LOGGER.finest("Setting value [location ="+location+"]");
                                    LOGGER.finest("NEW_LINE -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{CICLO}			{
                                    numCiclomatic++;
                                    numCiclomaticTotal++;
                                    LOGGER.finest("Setting values [numCiclomatic="+numCiclomatic+" | numCiclomaticTotal ="+numCiclomaticTotal+"]");
                                }
<NEW_LINE>		{CLOSING}		{}
<NEW_LINE>		{END}			{endLocation();}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{
                                    LOGGER.finest("NEW_LINE -> NEW_LINE (Transition : \\n)");
                                }
<NEW_LINE>  	.              	{
                                    LOGGER.finest("NEW_LINE -> LINE (Transition : .)");
                                    yybegin(LINE);
                                }


<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{
                                    location = yytext();
                                    LOGGER.finest("Setting value [location="+location+"]");
                                    LOGGER.finest("LINE -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<LINE>			{CICLO}			{
                                    numCiclomatic++;
                                    numCiclomaticTotal++;
                                    LOGGER.finest("Setting values [numCiclomatic="+numCiclomatic+" | numCiclomaticTotal ="+numCiclomaticTotal+"]");
                                }
<LINE>			{CLOSING}		{}
<LINE>			{END}			{endLocation();}
<LINE>			{VAR}			{}
<LINE>      	\n             	{
                                    LOGGER.finest("LINE -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}


				[^]            {
									
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
								}