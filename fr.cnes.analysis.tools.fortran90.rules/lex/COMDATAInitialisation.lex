/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Initialisation rule. */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/


package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInitialisation
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, NEW_LINE, LINE, INIT, VAR_EQ, WAIT, FUNC, AVOID, DATA, WAIT_ARRAY, PARAMS, ARRAY, DECLARATION

COMMENT_WORD = \! 
FREE_COMMENT = \!
COMMENT_LINE = \! [^\n]*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB}    | {PROG} | {MOD}
EQUAL        = \=
INTEGER      = INTEGER    | integer
LOGICAL		 = LOGICAL	  | logical
CHARAC		 = CHARACTER  | character
REAL		 = REAL		  | real
COMPLEX      = COMPLEX    | complex
DOUBLE		 = DOUBLE     | double
PREC		 = PRECISION  | precision
DOUBLE_PREC	 = {DOUBLE}([\ ]*){PREC}
STRUCT		 = TYPE[\ ]*\(| type[\ ]*\(
DIMENSION	 = "dimension"
DATA		 = "data"
INTENT		 = "intent" {SPACE}* \( 
INTENTIN		 = "intent" {SPACE}*\({SPACE}*"in"{SPACE}*\)|"INTENT" {SPACE}*\({SPACE}*"IN"{SPACE}*\)
READ		 = ([^a-zA-Z0-9\_])?"read"[^a-zA-Z0-9\_\n]
COMMON		 = ([^a-zA-Z0-9\_])?"common"[^a-zA-Z0-9\_\n]
NAMELIST	 = ([^a-zA-Z0-9\_])?"namelist"[^a-zA-Z0-9\_\n]
SAVE		 = ([^a-zA-Z0-9\_])?"save"[^a-zA-Z0-9\_\n] 
EQUIV		 = ([^a-zA-Z0-9\_])?"equivalence"[^a-zA-Z0-9\_\n] 
END			 = ([^a-zA-Z0-9\_])?"end"[^a-zA-Z0-9\_\n]
GOTO		 = ([^a-zA-Z0-9\_])?"go"[\ ]*"to"[^a-zA-Z0-9\_\n]
EXT			 = ([^a-zA-Z0-9\_])?"external"[^a-zA-Z0-9\_\n]
CALL		 = ([^a-zA-Z0-9\_])?"call"[^a-zA-Z0-9\_\n]
IMPLICIT	 = ([^a-zA-Z0-9\_])?"implicit"[^a-zA-Z0-9\_\n]
INITILIAZE 	 = {READ} 	| {DATA}	| {COMMON}		| {NAMELIST}	| {SAVE}	| {EQUIV}	| {CALL}
CLE			 = {END}	| {GOTO}	| {EXT}			| {IMPLICIT}
VAR_T     	 = {INTEGER}  | {LOGICAL}  | {CHARAC} | {REAL} | {COMPLEX} | {DOUBLE_PREC} | {STRUCT} 
VAR_PAR		 = {VAR}{SPACE}*\( 
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*
NUM			 = [0-9]+\.([0-9]*("e"|"d")(\-|\+)?[0-9]+)?((\_)?{VAR})?
POINT		 = (\%{VAR})+
SPACE		 = [\ \r\t\f]
STRING		 = \'[^\']*\' | \"[^\"]*\"
SIMBOL		 = \& 		  | \$ 		   | \+			| [A-Za-z][\ ]	| \.	| [0-9]
COMP		 = "/=" | "==" | ".eq." | ".ne." | ".NE." | ".EQ."
SEE_FUNC	 = ([^a-zA-Z0-9\_])?("if" | "elseif" | "forall" | "while" | "where" | "WHERE" | "FORALL" | "IF" | "ELSEIF" | "WHILE"){SPACE}*"("
																
%{
	String location = "MAIN PROGRAM";
	
	Map<String, Boolean> variables = new HashMap<String, Boolean>();
	List<String> dimension = new LinkedList<String>();
	
	//mantis EGL 316
	Map<String, List<String>> mVariableByCallType = new HashMap<String, List<String>>();
	Map<String, List<String>> mVariableByType = new HashMap<String, List<String>>();
	
	String variable = "";
	boolean initialized = false;
	int par = 0;
	boolean dim = false, fin = true;

	//mantis EGL 312/316/324/313
	boolean isPotentialError = false;
	//when a call is fire
	boolean isCall=false;
	boolean firstCall=false;
	//when an intent(out) is produce
	boolean isIntentout=false;
	//name of sunroutine
	String nameType="";

    public COMDATAInitialisation() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	//Mantis 316 EGL : detect initialized by local fonction
	/**
	 * This function add Naming subroutin, function, module in List  
	 * in order to analyse at the end of parsing if 
	 * this variable is initialized by this naming which called in source code.
	 * this function is call when a call of naming is done.
	 * @param funct
	 * @param paramVar
	 * @param position
	 */
	private void addVariableByCallType(final String funct, final String paramVar,final String location,final int line){
		//if variable is declared
		if(variables.containsKey(paramVar)){
			//if variable is not initialized
			//then add in this  mVariableByType to check if this type naming  
			//initialize it.
			if(!variables.get(paramVar)){

				List<String> param ;
				if (mVariableByCallType.get(funct)!=null){
					param = mVariableByCallType.get(funct);
				}else{					
					param = new ArrayList<String>();					
				}
				param.add("paramVar="+paramVar);
				param.add("location="+location);
				param.add("line="+line);
				param.add("error=false");

				mVariableByCallType.put(funct,param);

			}
		}
	}

	/**
	 * This function parse Naming subroutin, function, module in List  
	 * in order to analyse at the end of parsing if 
	 * this variable is initialized by this naming .
	 * this function is applied in type or naming .
	 * @param funct
	 * @param paramVar
	 * @param position
	 */
	private void addVariableByType(final String funct, final String paramVar){

		List<String> param ;
		if (mVariableByType.get(funct)!=null){
			param = mVariableByType.get(funct);										
		}else{					
			param = new ArrayList<String>();				
		}
		param.add("paramVar="+paramVar);
		mVariableByType.put(funct,param);

	}

	/**
	 * This fonction is recommended in intent treatment to update initialized state of variable
	 * @param funct
	 * @param paramVar
	 * @return true if this variable is updated as no initialized false else
	 */
	private boolean setErrorVariableByType(final String funct, final String paramVar){
		//test limitation of this rule with call function
		boolean ret = false;  
		if(mVariableByCallType.containsKey(funct)  && mVariableByType.containsKey(funct)){
			List<String> paramCall = mVariableByCallType.get(funct);
			List<String> param = mVariableByType.get(funct);
			if(param!=null && paramCall!=null ){
				final int indexParam = param.indexOf("paramVar="+paramVar);
				//if it is the same function cause of prototype function
				//then param are in same place in signature function
				if((indexParam > -1) && (paramCall.size() >(indexParam * 4))){
					final String sParamCall = paramCall.get(indexParam * 4);
					if(sParamCall.contains("paramVar=")){
						paramCall.set(indexParam + 3, "error=true");
						ret = true;
					}
				}
			}
		}
		return ret;
	}
	
	/**
	 * this function display add violation for variables which are not initialized in call function 
	 * @throws JFlexException
	 * @throws NumberFormatException
	 */
	private void displayErrorVariableByType() throws JFlexException,NumberFormatException {
		
		//add at the end all violation error 
		//function shall be described in this source code
		if(!mVariableByCallType.isEmpty()){
			final Set<Map.Entry<String,List<String>>> entryCall = mVariableByCallType.entrySet();
			
			
			for (Map.Entry<String,List<String>> ent:entryCall){
				
				final List<String> entList =  ent.getValue();			
				if(entList!=null && !entList.isEmpty())
				for(int i=0;i<entList.size();i++){				
					if(entList.get(i).contains("paramVar=")){
						final String error= entList.get(i+3).substring(entList.get(i+3).indexOf("=")+1, entList.get(i+3).length());
						final String paramVar= entList.get(i).substring(entList.get(i).indexOf("=")+1, entList.get(i).length());
						if("true".equals(error)){
							final String dLocation= entList.get(i+1).substring(entList.get(i+1).indexOf("=")+1, entList.get(i+1).length());
							final String line= entList.get(i+2).substring(entList.get(i+2).indexOf("=")+1, entList.get(i+2).length());									
							setError(dLocation,"The variable " + paramVar + " is used before being initialized. ", Integer.parseInt(line));
												 
						}
						
					}
					
				}
				
			}	
		}
	}
		
%}

%eofval{
//mantis 316
displayErrorVariableByType();
return getViolations();
%eofval}


%%          

/******************************************************************************/
/* This part deals with a "free" comment, which means not at the beginning of */
/* a line.																	  */
/******************************************************************************/
				{FREE_COMMENT}	{yybegin(COMMENT);}


/******************************************************************************/
/* This part deals with the comment section, to avoid any word on these lines.*/
/******************************************************************************/
/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n             	{yybegin(NEW_LINE);}  
<COMMENT>   	.              	{}

/************************/
/* AVOID STATE	   		*/
/************************/
<AVOID>			{COMMENT_LINE}		{}
<AVOID>			{SPACE}				{}
<AVOID>			\n[\ ]{1,5}{SIMBOL}	{}
<AVOID>   		\n             		{yybegin(NEW_LINE);}  
<AVOID>   		.              		{}

/******************************************************************************/
/* This part deals with function, procedure and other's name. It is 	 	  */
/* to determine, when it exists, the name of each one. Whenever a name  	  */
/* is encountered, the following part is ignored, moving to COMMENT state.    */
/******************************************************************************/
/************************/
/* NAMING STATE	        */
/************************/
<NAMING>		{COMMENT_LINE}	{}
<NAMING>		{VAR}			{nameType=yytext();location = location + " " + yytext(); variables.clear(); dimension.clear(); yybegin(PARAMS);}
<NAMING>    	\n             	{variables.clear(); dimension.clear(); yybegin(NEW_LINE);}
<NAMING>    	.              	{}


/************************/
/* PARAMS STATE	        */
/************************/
<PARAMS>		{COMMENT_LINE}		{}
<PARAMS>		{VAR}				{addVariableByType(nameType, yytext());variables.put(yytext(), true);}
<PARAMS>		{SPACE}				{}
<PARAMS>		\n[\ ]{1,5}{SIMBOL}	{}
<PARAMS>   		\n             		{yybegin(NEW_LINE);}  
<PARAMS>   		.              		{}

/******************************************************************************/
/* This is the first state of the automaton. The automaton will never go back */
/* to this state after.                                                       */
/******************************************************************************/
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{yybegin(COMMENT);}
<YYINITIAL>		{TYPE}        	{location = yytext(); yybegin(NAMING);}
<YYINITIAL>     {VAR_T}			{yybegin(DECLARATION);}
<YYINITIAL> 	\n             	{yybegin(NEW_LINE);}
<YYINITIAL> 	.              	{yybegin(LINE);}

/******************************************************************************/
/* This state is reached whenever a new line starts.                          */
/******************************************************************************/
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{COMMENT_WORD} 	{yybegin(COMMENT);}
<NEW_LINE>		{STRING}		{yybegin(LINE);}
<NEW_LINE>  	{TYPE}         	{location = yytext(); yybegin(NAMING);}
<NEW_LINE>		{INITILIAZE}	{
									if ("call".equals(yytext().trim().toLowerCase())){
									 isCall=true;	
									 firstCall=true;	
									 }							
									 yybegin(DATA);
									
								}
<NEW_LINE>      {VAR_T}			{yybegin(DECLARATION);}
<NEW_LINE>		{CLE}			{yybegin(AVOID);}
<NEW_LINE>		{SEE_FUNC}		{}
<NEW_LINE>	    {COMP}   		{
								  if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
                                }
<NEW_LINE>		{VAR_PAR}		{variable = yytext().substring(0,yytext().length()-1).trim();  par = 1;
								 if (!dimension.contains(variable)){
								 	 yybegin(FUNC);
								 		 
								 }else{
								 	 isPotentialError = true;
								 	 yybegin(ARRAY);
								 }
								 		
								}
<NEW_LINE>		{VAR}			{if(variables.containsKey(yytext())) { variable = yytext(); initialized = false; yybegin(VAR_EQ);} } 
<NEW_LINE>		{NUM}|{POINT}	{yybegin(LINE);}
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{yybegin(LINE);}


/******************************************************************************/
/* This state is whenever none of the others has been reached.                */
/******************************************************************************/
/************************/
/* LINE STATE           */
/************************/
<LINE>			{COMMENT_LINE} 	{yybegin(COMMENT);}
<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{location = yytext(); yybegin(NAMING);}
<LINE>			{INITILIAZE}	{
									if ("call".equals(yytext().trim().toLowerCase())){
									 isCall=true;
		                             firstCall=true;
		                             }							 
									 yybegin(DATA);
							    }
<LINE>   	    {VAR_T}			{yybegin(DECLARATION);}
<LINE>			{CLE}			{yybegin(AVOID);}
<LINE>	        {SEE_FUNC}		{}
<LINE>	        {COMP}   		{
								  if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
								}
<LINE>			{VAR_PAR}		{variable = yytext().substring(0,yytext().length()-1).trim();  par = 1;
								  if (!dimension.contains(variable)){
								        yybegin(FUNC);
								  }else{
								  		isPotentialError = true;
								        yybegin(ARRAY);
								  }
								}
<LINE>			{VAR}			{if(variables.containsKey(yytext())) { variable = yytext(); initialized = false; yybegin(VAR_EQ);} } 
<LINE>			{NUM}|{POINT}	{}
<LINE>			{EQUAL} 		{isPotentialError = false;;if(variables.containsKey(variable)) variables.put(variable, true ); }
<LINE>      	\n             	{if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
								  yybegin(NEW_LINE);
									
								}
<LINE>      	.            	{yybegin(NEW_LINE);}

/************************/
/* INIT STATE           */
/************************/
<INIT>			{COMMENT_LINE}		{}
<INIT>			{STRING}			{}
<INIT>		  	{VAR} 				{variable = yytext(); fin=true;
									 if (isIntentout){
									 	setErrorVariableByType(nameType, variable);		
									 							 
									 }	
								 	 if(!variables.containsKey(variable)) variables.put(variable, false);
								 	 if(dim) dimension.add(variable);}
<INIT>			{EQUAL}				{variables.put(variable, true ); yybegin(WAIT);}
<INIT>			{NUM}				{fin=true;}
<INIT>			\(					{par=1; yybegin(WAIT_ARRAY);}
<INIT>			{SPACE}				{}
<INIT>			\n[\ ]{1,5}{SIMBOL}	{}
<INIT>			\n					{dim=false; if(fin)yybegin(NEW_LINE);}
<INIT>			.					{isIntentout=false;fin=true;}

/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{TYPE}				{location = yytext(); yybegin(NAMING);}
<DECLARATION>	{STRING}	        {}
<DECLARATION>	{DIMENSION}		    {dim=true;}
<DECLARATION>	{INTENTIN}		    {dim=false; isIntentout=true;}
<DECLARATION>	{INTENT}		    {dim=false; yybegin(COMMENT);}
<DECLARATION>	\:\:			    {yybegin(INIT);}
<DECLARATION>	{VAR}[\ ]* \(		{variable = yytext().substring(0, yytext().length()-1).trim();  
									 if(!variables.containsKey(variable))variables.put(variable, false); 
									 dimension.add(variable);}
<DECLARATION>	{VAR}				{if(!variables.containsKey(yytext())) variables.put(yytext(), false);}
<DECLARATION>	{EQUAL}				{variables.put(variable, true ); yybegin(WAIT);}
<DECLARATION>	{NUM}				{}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}
<DECLARATION>  	\n             		{yybegin(NEW_LINE);}
<DECLARATION>  	.              		{}

/************************/
/* WAIT STATE           */
/************************/
<WAIT>			{COMMENT_LINE}	{}
<WAIT>			{STRING}		{}
<WAIT>			\,				{yybegin(DECLARATION);}
<WAIT>			\n				{yybegin(NEW_LINE);}
<WAIT>			.				{}

/************************/
/* WAIT_ARRAY STATE     */
/************************/
<WAIT_ARRAY>		{COMMENT_LINE}	{}
<WAIT_ARRAY>		{STRING}		{}
<WAIT_ARRAY>		\(				{par++;}
<WAIT_ARRAY>		\)				{par--; if(par==0) yybegin(DECLARATION);}
<WAIT_ARRAY>		[^] 			{}

/************************/
/* DATA STATE           */
/************************/
<DATA>			{COMMENT_LINE}		{}
<DATA>			{STRING}			{}
<DATA>			{VAR}				{
										if (firstCall){
										 nameType=yytext();
										 firstCall=false;
										}	
										if(isCall){
										    final String var = yytext();
											addVariableByCallType(nameType, var, location,yyline +1);
										}						
										variables.put(yytext(), true)
										; fin=true;
									}
<DATA>			{SPACE}				{}
<DATA>			\n[\ ]{1,5}{SIMBOL}	{}
<DATA>   		\n             		{isCall=false;yybegin(NEW_LINE);}  
<DATA>			.					{}

/************************/
/* FUNC STATE           */
/************************/
<FUNC>			{COMMENT_LINE}	{}
<FUNC>			{STRING}		{}
<FUNC>			{VAR}			{String var = yytext();
								 if(variables.containsKey(var)) variables.put(var, true);}
<FUNC>			\(				{par++;}
<FUNC>			\)				{par--; if(par==0) yybegin(LINE);}
<FUNC>			[^] 			{}

/************************/
/* ARRAY STATE           */
/************************/
<ARRAY>			{COMMENT_LINE}	{}
<ARRAY>			{STRING}		{}
<ARRAY>			{VAR}			{String var = yytext();
								 Boolean init = variables.get(var);
								 if(init != null && !init) setError(location,"The variable " + var + " is used before being initialized. ", yyline+1); }
<ARRAY>			\(				{par++;}
<ARRAY>			\)				{par--; if(par==0) yybegin(LINE);}
<ARRAY>			[^] 			{}

/************************/
/* VAR_EQ STATE         */
/************************/
<VAR_EQ>		{COMMENT_LINE}	{}
<VAR_EQ>		{STRING}		{}
<VAR_EQ>	    {SEE_FUNC}		{}
<VAR_EQ>		{VAR_PAR}		{String var = yytext().substring(0,yytext().length()-1).trim();
								 if (!dimension.contains(var)) { par = 1; yybegin(FUNC);}  }
<VAR_EQ>		{VAR}			{variable = yytext(); initialized = false;}
<VAR_EQ>		{EQUAL} 		{initialized = true;
								 variables.put(variable, true ); }
<VAR_EQ>		{COMP}          {}
<VAR_EQ>		{SPACE}			{}
<VAR_EQ>		{NUM}|{POINT}	{}
<VAR_EQ>		\n				{if(!initialized) {
									Boolean init = variables.get(variable);
									if(init != null && !init) setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1);
								 } 
								 initialized = false;
								 yybegin(NEW_LINE);}
<VAR_EQ>		.				{if(!initialized) {
									Boolean init = variables.get(variable);
									if(init != null && !init) setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1);
								 } 
								 initialized = false; variable = yytext();
								}

/************************/
/* ERROR STATE	        */
/************************/
				[^]            {throw new JFlexException( new Exception("Illegal character <" + yytext() + "> at line :"+yyline) );}