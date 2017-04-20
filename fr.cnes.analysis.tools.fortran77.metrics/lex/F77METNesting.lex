/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                              */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a metric checker for comment's rate. For 		*/
/* further information on this, we advise you to refer to CNES manual dealing	*/
/* with metrics.																*/
/* As many comments have been done on the MAXImbric.lex file, this file 		*/
/* will restrain its comments on modifications.									*/
/*																				*/
/********************************************************************************/

package fr.cnes.analysis.tools.fortran77.metrics;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

%%

%class F77METNesting
%extends AbstractMetric
%public
%ignorecase
%line
%column

%function run
%yylexthrow JFlexException
%type FileValue

%state COMMENT, NAMING, NEW_LINE, LINE, IF_STATE

/* We add TYPE notion, which represent FUNC, PROC, SUB, MOD and PROG. 	*/
/* We also add END, which is used to ignore end of function, etc.	*/
COMMENT_WORD = \!         | c          | C     | \*
FUNC         = "FUNCTION "
PROC         = "PROCEDURE "
SUB          = "SUBROUTINE "
PROG         = "PROGRAM "
MOD          = "MODULE "
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD}
END			 = END		  | end
DO		   	 = DO		  | do
IF			 = IF    	  | if		
THEN		 = THEN		  | then
ELSES        = ELSE[\ ]*IF| else[\ ]*if | ELSE | else
END_CONTROL  = END[\ ]*IF |end[\ ]*if | [0-9]+[\ ]+CONTINUE | [0-9]+[\ ]+continue |
			   END[\ ]*DO | end[\ ]*do
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t]+
SMBL		 = \&   	  | \+			| \$  


%{

    private static final Logger LOGGER = Logger.getLogger(F77METNesting.class.getName());
    

	String location = "MAIN PROGRAM";
	FileValue fileValue;
	List<String> identifiers = new LinkedList<String>();
	float numImbrics = 0;
	float numMaxImbrics = 0;
	float numImbricsTotal = 0;
	boolean end = true;
	int functionLine = 0;
	String parsedFileName;
	
	public F77METNesting(){ 
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		LOGGER.info("l"+(yyline+1)+" -begin method setInputFile");
		fileValue = new FileValue(this.getContribution().getAttribute("id"), this.getContribution().getAttribute("name"), file);
		this.parsedFileName = file.toString();
		this.zzReader = new FileReader(file.toOSString());
		LOGGER.info("l"+(yyline+1)+" -end method setInputFile");
	}
	
	private void addImbrics() {
		LOGGER.info("l"+(yyline+1)+" -begin method addImbrics");
		numImbrics++;
		if(numMaxImbrics < numImbrics)  {
			numMaxImbrics = numImbrics;
			numImbricsTotal = numImbrics;
		}
		LOGGER.info("l"+(yyline+1)+" -end method addImbrics");
	}
	
	private void deleteImbrics() {
		LOGGER.info("l"+(yyline+1)+" -begin method deleteImbrics");
		numImbrics--;
		LOGGER.info("l"+(yyline+1)+" -end method deleteImbrics");
	}
	
	private void endLocation() {
	    LOGGER.info("l"+(yyline+1)+" -begin method endLocation");
		final List<FunctionValue> list =
                this.fileValue.getFunctionValues();
        if (list.isEmpty()) {
            list.add(new FunctionValue(this.location, numMaxImbrics, functionLine+1));
        } else {
            final FunctionValue last = list.get(list.size() - 1);
            if (last.getLocation().equals(this.location)) {
                last.setValue(numMaxImbrics);
            } else {
				list.add(new FunctionValue(this.location, numMaxImbrics, functionLine+1));
			}
        }
        LOGGER.info("l"+(yyline+1)+" -end method endLocation");
	}
	
%}

%eofval{
	return fileValue;
%eofval}

%%

/* This is the general automaton. Each part will be described later. */

				{COMMENT_WORD}	{
				                    LOGGER.info("l"+(yyline+1)+" -[ALL] -> COMMENT (Transition : COMMENT_WORD)");
				                    yybegin(COMMENT);
				                }
				
				
<COMMENT>   	\n             	{
                                    LOGGER.info("l"+(yyline+1)+" -COMMENT -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }  
<COMMENT>   	.              	{}


<NAMING>		{VAR}			{
                                    numImbrics = 0;
                                    numMaxImbrics = 0;
                                    functionLine=yyline;
                                    location = location + " " + yytext();
                                    LOGGER.info("l"+(yyline+1)+" -Setting values [numImbrics ="+numImbrics+" | numMaxImbrics = "+ numMaxImbrics+" | location = "+location+"]");
                                    LOGGER.info("l"+(yyline+1)+" -NAMING -> COMMENT (Transition : VAR)");
                                    yybegin(COMMENT);
                                }							 
<NAMING>    	\n             	{
                                    LOGGER.info("l"+(yyline+1)+" -NAMING -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


<YYINITIAL>		{STRING}		{
                                    LOGGER.info("l"+(yyline+1)+" -YYINITIAL -> LINE (Transition : STRING)");
                                    yybegin(LINE);
                                }
<YYINITIAL>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.info("l"+(yyline+1)+" -Setting value [location = "+location+"]");
                                    LOGGER.info("l"+(yyline+1)+" -YYINITIAL -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.info("l"+(yyline+1)+" -YYINITIAL -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.info("l"+(yyline+1)+" -YYINITIAL -> LINE (Transition : .)");
                                    yybegin(LINE);
                                }

<NEW_LINE>		{STRING}		{
                                    LOGGER.info("l"+(yyline+1)+" -NEW_LINE -> LINE (Transition : STRING)");
                                    yybegin(LINE);
                                }
<NEW_LINE>  	{TYPE}         	{
                                    
                                    location = yytext();
                                    LOGGER.info("l"+(yyline+1)+" -Setting value [location = "+location+"]");
                                    LOGGER.info("l"+(yyline+1)+" -NEW_LINE -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{ELSES}			{}
<NEW_LINE>		{DO}			{
                                    addImbrics();
                                }
<NEW_LINE>		{IF}			{
                                    LOGGER.info("l"+(yyline+1)+" -NEW_LINE -> IF_STATE (Transition : IF)");
                                    yybegin(IF_STATE);
                                }
<NEW_LINE>		{END_CONTROL}	{
                                    deleteImbrics();
                                }
<NEW_LINE>		{END}			{
                                    endLocation();
                                }
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.info("l"+(yyline+1)+" -NEW_LINE -> LINE (Transition : .)");
                                    yybegin(LINE);
                                }

<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{
                                    location = yytext();
                                    LOGGER.info("l"+(yyline+1)+" -LINE -> NAMING (Transition : TYPE)");
                                    yybegin(NAMING);
                                }
<LINE>			{ELSES}			{}
<LINE>			{DO}			{
                                    addImbrics();
                                }
<LINE>			{IF}			{
                                    LOGGER.info("l"+(yyline+1)+" -LINE -> IF_STATE (Transition : IF)");
                                    yybegin(IF_STATE);
                                }
<LINE>			{END_CONTROL}	{
                                    deleteImbrics();
                                }
<LINE>			{END}			{
                                    endLocation();
                                }
<LINE>			{VAR}			{}
<LINE>      	\n             	{
                                    LOGGER.info("l"+(yyline+1)+" -LINE -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<LINE>      	.              	{}

<IF_STATE>		{THEN} 		    {
                                    addImbrics();
                                    LOGGER.info("l"+(yyline+1)+" -IF_STATE -> LINE (Transition : THEN)");
                                    yybegin(LINE);
                                }
<IF_STATE>		{VAR}			{}
<IF_STATE>		\n{SPACE}{SMBL} {}
<IF_STATE>		\n 				{
                                    addImbrics();
                                    deleteImbrics();
                                    LOGGER.info("l"+(yyline+1)+" -IF_STATE -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<IF_STATE>		.				{}



				[^]            {
				                    String errorMessage = "Class"+this.getClass().getName()+"\nIllegal character <" + yytext() + ">\nFile :"+ this.parsedFileName+"\nat line:"+yyline+" column:"+yycolumn;
                                    throw new JFlexException(new Exception(errorMessage));
                               }
