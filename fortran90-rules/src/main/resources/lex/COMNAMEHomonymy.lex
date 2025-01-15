/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.NAME.Homonime rule. 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/*                                                                              */
/********************************************************************************/

package fr.cnes.icode.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;

%%

%class COMNAMEHomonymy
%extends AbstractChecker
%public
%column
%line

%ignorecase

%function run
%yylexthrow JFlexException
%type List<CheckResult>


%state COMMENT, NAMING, PARAMS, NEW_LINE, LINE, DECLARATION, AVOID_DECL, AVOID, TYPE_DEC, DECL_PARAMS, NOTHING

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
REC          = RECURSIVE  | recursive
ELE          = ELEMENTAL  | elemental
PUR			 = PURE 	  | pure
MODIF		 = {REC} | {ELE} | {PUR} 
TYPE		 = {FUNC}     | {PROC}	   | {SUB} | {PROG} | {MOD} | 
			   {DATA_TYPE}[\ ]+{FUNC}  | ({MODIF}{SPACE})*({SUB} | {FUNC})
DATA_TYPE	 = INTEGER |integer | LOGICAL | logical | CHARACTER | character |
               REAL | real | COMPLEX | complex | DOUBLE[\ ]+PRECISION |
               double[\ ]+precision| CHARACTER{SPACE}*"\*"{SPACE}*"("{SPACE}*{VAR}{SPACE}*")" | 
               character{SPACE}*"\*"{SPACE}*"("{SPACE}*{VAR}{SPACE}*")" 
END			 = END		  | end
END_TYPE	 = {END} [\ ]* {TYPE}
STRUCT		 = TYPE		  | type
END_STRUCT	 = {END} [\ ]* {STRUCT}
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
STRING		 = \'[^\']*\' | \"[^\"]*\"
SPACE		 = [\ \r\t\f]
																
%{
	String location = "MAIN PROGRAM";
    private String parsedFileName;
	Map<String, List<String>> hierarchy = new HashMap<String, List<String>>();
	Map<String, List<String>> variables = new HashMap<String, List<String>>();
	List<String> locOrder = new LinkedList<String>();
	List<String> parameters = new LinkedList<String>();
	int par = 0;
	boolean end = true, endStruct = true;
	
	
	public COMNAMEHomonymy(){
		locOrder.add(location);
	}
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new File(file.getAbsolutePath()));
	}
	
	
	
	private void chechHomonymy(String var) throws JFlexException {
		String loc = location;
		boolean found = false;
		while (loc!=null && !found) {
			List<String> list = variables.get(loc);
			if(list != null) {
				if(list.contains(var) && !parameters.contains(var)) {
					setError(location,"Variable names should be unique. The variable " + var + " is already defined in this file.", yyline+1);
					found = true;
				}
			}
			if(!found) loc = findParent(loc);
		}
	}
	
	private String findParent(String currentLoc) {
		Iterator<Entry<String, List<String>>> it = hierarchy.entrySet().iterator();
		while (it.hasNext()) {
			Entry<String, List<String>> pairs = it.next();
			List<String> list = pairs.getValue();
			if(list.contains(currentLoc)) return pairs.getKey();
		}
		return null;
	}


	
%}

%eofval{
    
	
	return getCheckResults();
%eofval}
%eofclose

%%          

/************************/

				{FREE_COMMENT}	{yybegin(COMMENT);}
/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{if(endStruct) yybegin(NEW_LINE);
								 else yybegin(AVOID);}  
<COMMENT>   	.              	{}


/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{FREE_COMMENT}	{yybegin(COMMENT);}
<NAMING>		{VAR}			{String text = yytext().toLowerCase().trim();
								 if(!location.toLowerCase().trim().equals("module") || !text.equals("procedure")) {
								 	location = location + " " + text; 
								 	locOrder.add(location); String oldLoc = locOrder.get(locOrder.size()-2);
								 	List<String> list = hierarchy.get(oldLoc);
								 	if(list==null) list = new LinkedList<String>();
								 	list.add(location);
								 	hierarchy.put(oldLoc, list);
								 }
								 parameters.clear();
								 yybegin(PARAMS);
								}
<NAMING>    	\n             	{yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* PARAMS STATE		    */
/************************/
<PARAMS>  		{VAR}		 	{parameters.add(yytext());}
<PARAMS>		&				{end=false;}
<PARAMS>		{SPACE}			{}
<PARAMS> 		\n             	{if(end)yybegin(NEW_LINE);}
<PARAMS> 		.              	{end=true;}


/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}


/************************/
/* NEW_LINE STATE	    */
/************************/
<NEW_LINE>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{}
<NEW_LINE>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{END_TYPE}		{
									if(locOrder.isEmpty()){
											
				                    		final String errorMessage = "Analysis failure : Location unreachable.";
					                    	throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
									}
									locOrder.remove(locOrder.size()-1);}
<NEW_LINE>		{DATA_TYPE}		{par=0; yybegin(DECL_PARAMS);}
<NEW_LINE>		{STRUCT}		{yybegin(TYPE_DEC);}
<NEW_LINE>		{VAR}			{}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/************************/
/* LINE STATE    	    */
/************************/
<LINE>			{STRING}		{}
<LINE>			{TYPE}        	{location = yytext(); yybegin(NAMING);}
<LINE>			{END_TYPE}		{
									if(locOrder.isEmpty()){
											
				                    		final String errorMessage = "Analysis failure : Location unreachable.";
					                    	throw new JFlexException(this.getClass().getName(), parsedFileName,
                                    errorMessage, yytext(), yyline, yycolumn);
									}
									locOrder.remove(locOrder.size()-1);}
<LINE>			{DATA_TYPE}		{par=0; yybegin(DECL_PARAMS);}
<LINE>			{STRUCT}		{yybegin(TYPE_DEC);}
<LINE>			{VAR}			{yybegin(NOTHING);}
<LINE>			{SPACE}			{}
<LINE>      	\n             	{yybegin(NEW_LINE);}
<LINE>      	.              	{yybegin(NOTHING);}



/************************/
/* DECL_PARAMS STATE    */
/************************/
<DECL_PARAMS>		{TYPE}			{location = yytext(); yybegin(NAMING);}
<DECL_PARAMS>		\:\:			{yybegin(DECLARATION);}
<DECL_PARAMS>		\n				{yybegin(NEW_LINE);}
<DECL_PARAMS>		.				{}


/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	\(				{par++; end=true;}
<DECLARATION>	\)				{par--; end=true;}
<DECLARATION>	{VAR}			{end=true; chechHomonymy(yytext());
								 List<String> list = variables.get(location);
								 if(list==null) list = new LinkedList<String>();
								 list.add(yytext());
								 variables.put(location, list);
								 yybegin(AVOID_DECL);
								}
<DECLARATION>	&				{end=false;}
<DECLARATION>	\n             	{if(end) yybegin(NEW_LINE);}
<DECLARATION>	{SPACE}			{}
<DECLARATION>	.              	{end=true;}

/************************/
/* AVOID_DECL STATE    */
/************************/
<AVOID_DECL>	\(				{par++; end=true;}
<AVOID_DECL>	\)				{par--; end=true;}
<AVOID_DECL>	\,				{end=true; if(par==0) yybegin(DECLARATION);}
<AVOID_DECL>	&				{end=false;}
<AVOID_DECL>	\n             	{if(end) yybegin(NEW_LINE);}
<AVOID_DECL>	{SPACE}			{}
<AVOID_DECL>	.              	{end=true;}


/************************/
/* TYPE_DEC STATE	 	*/
/************************/
<TYPE_DEC>		\(				{yybegin(DECL_PARAMS);}
<TYPE_DEC>		{VAR}			{chechHomonymy(yytext());
								 List<String> list = variables.get(location);
								 if(list==null) list = new LinkedList<String>();
								 list.add(yytext());
								 variables.put(location, list);
								 endStruct = false;
								 yybegin(AVOID);}
<TYPE_DEC>		. 	         	{}
<TYPE_DEC>		\n				{yybegin(NEW_LINE);}


/************************/
/* AVOID STATE		    */
/************************/
<AVOID>			{END_STRUCT}   	{endStruct = true;}
<AVOID>			\n				{if(endStruct) yybegin(NEW_LINE);}
<AVOID>			. 	         	{}

/************************/
/* NOTHING STATE	    */
/************************/
<NOTHING>		&			   	{end = false;}
<NOTHING>		{SPACE}			{}
<NOTHING>		\n				{if(end) yybegin(NEW_LINE);}
<NOTHING>		. 	         	{end = true;}


/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
                                    
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, yytext(), yyline, yycolumn);
                                }