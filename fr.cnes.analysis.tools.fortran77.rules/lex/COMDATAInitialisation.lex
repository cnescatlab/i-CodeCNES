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

package fr.cnes.analysis.tools.fortran77.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMDATAInitialisation
%extends AbstractChecker
%public
%line
%ignorecase
%column


%function run
%yylexthrow JFlexException
%type List<CheckResult>

/* These are the states declaration for the automaton used at the end of this	*/
/* code. These states represents, when it's a comment section, when it's moving */
/* from a function to a module for instance (NAMING), when a new line starts    */
/* and when nothing special is happening (LINE). These states are not supposed 	*/
/* to be deleted. However, some modifications can be made to the transitions    */
%state COMMENT, NAMING, NEW_LINE, LINE, INIT, VAR_EQ, WAIT, FUNC, AVOID, DATA, WAIT_ARRAY, PARAMS, ARRAY, DECLARATION

COMMENT_WORD = \!         | c          | C     | \*
FREE_COMMENT = \!
COMMENT_LINE = \! [^\n]*
FUNC         = FUNCTION   | function
PROC         = PROCEDURE  | procedure
SUB          = SUBROUTINE | subroutine
PROG         = PROGRAM    | program
MOD          = MODULE     | module
TYPE		 = {FUNC}     | {PROC}	   | {SUB}    | {PROG} | {MOD}
EQUAL        = \=
COMP		 = "/=" | "==" | ".eq." | ".ne." | ".NE." | ".EQ."
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
SEE_FUNC	 = ([^a-zA-Z0-9\_])?("if" | "elseif" | "forall" | "while" | "where" | "WHERE" | "FORALL" | "IF" | "ELSEIF" | "WHILE"){SPACE}*"("

/* Variable "location" is used to determine rule's error location (function,	*/
/* procedure, etc.).															*/
/* A constructor without parameters is defined, in order to allow flexibility 	*/
/* with plug-in notion in Eclipse. As the original constructor needs a file 	*/
/* reader, setInputFile function is added, to allow definition of this reader.  */
/* A method called setError with String and integer parameters is used to store	*/
/* an error found during analysis.		     									*/
																	
%{
	private static final Logger LOGGER = Logger.getLogger(COMDATAInitialisation.class.getName());
	
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
	//name of the input file
	String parsedFileName;
	

    public COMDATAInitialisation() {
    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
        LOGGER.finest("begin method setInputFile");
        
		this.parsedFileName = file.toString();
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
        LOGGER.finest("end method setInputFile");
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
        LOGGER.finest("begin method addVariableByCallType");
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
        LOGGER.finest("begin method addVariableByCallType");
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
        LOGGER.finest("begin method addVariableByType");



		List<String> param ;
		if (mVariableByType.get(funct)!=null){
			param = mVariableByType.get(funct);										
		}else{					
			param = new ArrayList<String>();				
		}
		param.add("paramVar="+paramVar);
		mVariableByType.put(funct,param);
        LOGGER.finest("end method addVariableByType");
	}

	/**
	 * This fonction is recommended in intent treatment to update initialized state of variable
	 * @param funct
	 * @param paramVar
	 * @return true if this variable is updated as no initialized false else
	 */
	private boolean setErrorVariableByType(final String funct, final String paramVar){
        LOGGER.finest("begin method setErrorVariableByType");
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
		LOGGER.finest("end method setErrorVariableByType");
		return ret;
	}
	
	/**
	 * this function display add violation for variables which are not initialized in call function 
	 * @throws JFlexException
	 * @throws NumberFormatException
	 */
	private void displayErrorVariableByType() throws JFlexException,NumberFormatException {
		LOGGER.finest("begin method displayErrorVariableByType");
		//add at the end all violation error 
		//function shall be described in this source code
		if(!mVariableByCallType.isEmpty()){
			final Set<Map.Entry<String,List<String>>> entryCall = mVariableByCallType.entrySet();
			
			
			for (Map.Entry<String,List<String>> ent:entryCall){
				
				final List<String> entList =  ent.getValue();			
				if(entList!=null && !entList.isEmpty())
				for(int i=0;i<entList.size();i++){				
					if(entList.get(i).contains("paramVar=")){
						if(i+3 < entList.size()){
							final String error= entList.get(i+3).substring(entList.get(i+3).indexOf("=")+1, entList.get(i+3).length());
							final String paramVar= entList.get(i).substring(entList.get(i).indexOf("=")+1, entList.get(i).length());
							if("true".equals(error)){
								final String dLocation= entList.get(i+1).substring(entList.get(i+1).indexOf("=")+1, entList.get(i+1).length());
								final String line= entList.get(i+2).substring(entList.get(i+2).indexOf("=")+1, entList.get(i+2).length());
								LOGGER.fine("Setting error line "+line+" for the variable "+paramVar+".");								
								setError(dLocation,"The variable " + paramVar + " is used before being initialized. ", Integer.parseInt(line));
													 
							}
						}else{
							String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				            final String errorMessage = "Excepted parameter of "+ entList.get(i) +" unreachable.";
				            throw new JFlexException(this.getClass().getName(), parsedFileName,
				                            errorMessage, parsedWord, yyline, yycolumn);
						}
						
					}
					
				}
				
			}	
		}
        LOGGER.finest("end method displayErrorVariableByType");
	}
		
%}

/* At the end of analysis, atEOF is set at true. This is not meant to be modified. */
%eofval{
//mantis 316
displayErrorVariableByType();
return getCheckResults();
%eofval}


%%          

/************************/

/******************************************************************************/
/* This part deals with a "free" comment, which means not at the beginning of */
/* a line.																	  */
/******************************************************************************/
				{FREE_COMMENT}	{
				                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - [ALL] -> COMMENT (Transition : FREE_COMMENT \""+yytext()+"\")");
				                    yybegin(COMMENT);
				                }


/******************************************************************************/
/* This part deals with the comment section, to avoid any word on these lines.*/
/******************************************************************************/
/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	\n              	{
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - COMMENT -> NEW_LINE (Transition : \\n)");
                                        yybegin(NEW_LINE);
                                    }  
<COMMENT>   	.                	{}

/************************/
/* AVOID STATE	   		*/
/************************/
<AVOID>			{COMMENT_LINE}		{}
<AVOID>			{SPACE}				{}
<AVOID>			\n[\ ]{1,5}{SIMBOL}	{}
<AVOID>   		\n             		{
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - AVOID -> NEW_LINE (Transition : \\n)");
                                        yybegin(NEW_LINE);
                                    }  
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
<NAMING>		{VAR}			{
                                    nameType=yytext();location = location + " " + yytext(); variables.clear(); dimension.clear(); 
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> PARAMS (Transition : VAR \""+yytext()+"\" )");
                                    yybegin(PARAMS);
                                }
<NAMING>    	\n             	{
                                    variables.clear(); dimension.clear();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NAMING -> NEW_LINE (Transition : \\n)");
                                    yybegin(NEW_LINE);
                                }
<NAMING>    	.              	{}


/************************/
/* PARAMS STATE	        */
/************************/
<PARAMS>		{COMMENT_LINE}		{}
<PARAMS>		{VAR}				{addVariableByType(nameType, yytext());variables.put(yytext(), true);}
<PARAMS>		{SPACE}				{}
<PARAMS>		\n[\ ]{1,5}{SIMBOL}	{}
<PARAMS>   		\n             		{
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - PARAMS -> NEW_LINE (Transition : \\n)");
                                        yybegin(NEW_LINE);
                                    }  
<PARAMS>   		.              		{}

/******************************************************************************/
/* This is the first state of the automaton. The automaton will never go back */
/* to this state after.                                                       */
/******************************************************************************/
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>  	{COMMENT_WORD} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<YYINITIAL>		{TYPE}        	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<YYINITIAL>     {VAR_T}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> DECLARATION (Transition : VAR_T \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<YYINITIAL> 	\n             	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<YYINITIAL> 	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - YYINITIAL -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/******************************************************************************/
/* This state is reached whenever a new line starts.                          */
/******************************************************************************/
/************************/
/* NEW_LINE STATE       */
/************************/
<NEW_LINE>		{COMMENT_WORD} 	{
                                     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> COMMENT (Transition : COMMENT_WORD \""+yytext()+"\" )");
                                     yybegin(COMMENT);
                                }
<NEW_LINE>		{STRING}		{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : STRING \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>  	{TYPE}         	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<NEW_LINE>		{INITILIAZE}	{
									if ("call".equals(yytext().trim().toLowerCase())){
									 isCall=true;	
									 firstCall=true;	
									 }
									 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DATA (Transition : INITILIAZE \""+yytext()+"\" )");							
									 yybegin(DATA);
									
								}
<NEW_LINE>      {VAR_T}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> DECLARATION (Transition : VAR_T \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<NEW_LINE>		{CLE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> AVOID (Transition : CLE \""+yytext()+"\" )");
                                    yybegin(AVOID);
                                }
<NEW_LINE>		{SEE_FUNC}		{}
<NEW_LINE>	    {COMP}   		{
								  if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								  		LOGGER.fine("Setting error line "+yyline+1+" for the variable "+variable+".");    
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
                                }
<NEW_LINE>		{VAR_PAR}		{variable = yytext().substring(0,yytext().length()-1).trim();  par = 1;
								 if (!dimension.contains(variable)){
								     LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> FUNC (Transition : VAR_PAR \""+yytext()+"\" )");
								 	 yybegin(FUNC);
								 		 
								 }else{
								 	 isPotentialError = true;
								 	 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> ARRAY (Transition : VAR_PAR \""+yytext()+"\" )");
								 	 yybegin(ARRAY);
								 }
								 		
								}
<NEW_LINE>		{VAR}			{
                                    if(variables.containsKey(yytext())) {
                                        variable = yytext();
                                        initialized = false;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> VAR_EQ (Transition : VAR \""+yytext()+"\" )");
                                        yybegin(VAR_EQ);
                                    } 
                                } 
<NEW_LINE>		{NUM}|{POINT}	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : NUM | POINT  \""+yytext()+"\" )");
                                    yybegin(LINE);
                                }
<NEW_LINE>  	\n             	{}
<NEW_LINE>  	.              	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - NEW_LINE -> LINE (Transition : . )");
                                    yybegin(LINE);
                                }


/******************************************************************************/
/* This state is whenever none of the others has been reached.                */
/******************************************************************************/
/************************/
/* LINE STATE           */
/************************/
<LINE>			{COMMENT_LINE} 	{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> COMMENT (Transition : COMMENT_LINE  \""+yytext()+"\" )");
                                    yybegin(COMMENT);
                                }
<LINE>			{STRING}		{}
<LINE>  		{TYPE}         	{
                                    location = yytext();
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                    yybegin(NAMING);
                                }
<LINE>			{INITILIAZE}	{
									if ("call".equals(yytext().trim().toLowerCase())){
									 isCall=true;
		                             firstCall=true;
		                             }							 
									 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DATA (Transition : INITILIAZE \""+yytext()+"\" )");
									 yybegin(DATA);
							    }
<LINE>   	    {VAR_T}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> DECLARATION (Transition : VAR_T \""+yytext()+"\" )");
                                    yybegin(DECLARATION);
                                }
<LINE>			{CLE}			{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> AVOID (Transition : CLE \""+yytext()+"\" )");
                                    yybegin(AVOID);
                                }
<LINE>	        {SEE_FUNC}		{}
<LINE>	        {COMP}   		{
								  if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								 	    LOGGER.fine("Setting error line "+yyline+1+" for the variable "+variable+".");   
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
								}
<LINE>			{VAR_PAR}		{variable = yytext().substring(0,yytext().length()-1).trim();  par = 1;
								  if (!dimension.contains(variable)){
								        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> FUNC (Transition : VAR_PAR \""+yytext()+"\" )");
								        yybegin(FUNC);
								  }else{
								  		isPotentialError = true;
								  		LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> ARRAY (Transition : VAR_PAR \""+yytext()+"\" )");
								        yybegin(ARRAY);
								  }
								}
<LINE>			{VAR}			{
                                    if(variables.containsKey(yytext())) {
                                        variable = yytext();
                                        initialized = false;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> VAR_EQ (Transition : VAR \""+yytext()+"\" )");
                                        yybegin(VAR_EQ);
                                    }
                                } 
<LINE>			{NUM}|{POINT}	{}
<LINE>			{EQUAL} 		{isPotentialError = false;if(variables.containsKey(variable)) variables.put(variable, true ); }
<LINE>      	\n             	{if(isPotentialError && variables.containsKey(variable)){
									Boolean init = variables.get(variable);
								 	if(init != null && !init){
								  		LOGGER.fine("Setting error line "+yyline+1+" for the variable "+variable+"."); 
								  		setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1); 
									}
									isPotentialError=false;
								  }
								  LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - LINE -> NEW_LINE (Transition : \\n )");
								  yybegin(NEW_LINE);
									
								}
<LINE>      	.              	{}


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
<INIT>			{EQUAL}				{
                                        variables.put(variable, true );
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> WAIT (Transition : EQUAL \""+yytext()+"\" )");
                                        yybegin(WAIT);
                                    }
<INIT>			{NUM}				{fin=true;}
<INIT>			\(					{
                                        par=1;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> WAIT_ARRAY (Transition : [(] )");
                                        yybegin(WAIT_ARRAY);
                                    }
<INIT>			{SPACE}				{}
<INIT>			\n[\ ]{1,5}{SIMBOL}	{}
<INIT>			\n					{
                                        dim=false;
                                        if(fin){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - INIT -> NEW_LINE (Transition : \\n )");
                                            yybegin(NEW_LINE);
                                        }
                                    }
<INIT>			.					{isIntentout=false;	fin=true;}

/************************/
/* DECLARATION STATE    */
/************************/
<DECLARATION>	{TYPE}				{
                                        location = yytext();
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NAMING (Transition : TYPE \""+yytext()+"\" )");
                                        yybegin(NAMING);
                                    }
<DECLARATION>	{STRING}	        {}
<DECLARATION>	{DIMENSION}		    {dim=true;}
<DECLARATION>	{INTENTIN}		    {dim=false; isIntentout=true;}
<DECLARATION>	{INTENT}		    {
                                        dim=false;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> COMMENT (Transition : INTENT \""+yytext()+"\" )");
                                        yybegin(COMMENT);
                                    }
<DECLARATION>	\:\:			    {
										LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> INIT (Transition : [::] )");
										yybegin(INIT);
									}
<DECLARATION>	{VAR}[\ ]* \(		{variable = yytext().substring(0, yytext().length()-1).trim();  
									 if(!variables.containsKey(variable))variables.put(variable, false); 
									 dimension.add(variable);}
<DECLARATION>	{VAR}				{if(!variables.containsKey(yytext())) variables.put(yytext(), false);}
<DECLARATION>	{EQUAL}				{
                                        variables.put(variable, true );
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> WAIT (Transition : EQUAL \""+yytext()+"\" )");
                                        yybegin(WAIT);
                                    }
<DECLARATION>	{NUM}				{}
<DECLARATION>	\n[\ ]{1,5}{SIMBOL}	{}
<DECLARATION>  	\n             		{
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DECLARATION -> NEW_LINE (Transition : \\n )");
                                        yybegin(NEW_LINE);
                                    }
<DECLARATION>  	.              		{}

/************************/
/* WAIT STATE           */
/************************/
<WAIT>			{COMMENT_LINE}	{}
<WAIT>			{STRING}		{}
<WAIT>			\,				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT -> DECLARATION (Transition : [,] )");
                                    yybegin(DECLARATION);
                                }
<WAIT>			\n				{
                                    LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT -> NEW_LINE (Transition : \\n )");
                                    yybegin(NEW_LINE);
                                }
<WAIT>			.				{}

/************************/
/* WAIT_ARRAY STATE     */
/************************/
<WAIT_ARRAY>		{COMMENT_LINE}	{}
<WAIT_ARRAY>		{STRING}		{}
<WAIT_ARRAY>		\(				{par++;}
<WAIT_ARRAY>		\)				{
                                        par--;
                                        if(par==0){
                                            LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - WAIT_ARRAY -> DECLARATION (Transition : [)] )");
                                            yybegin(DECLARATION);
                                        }
                                    }
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
<DATA>   		\n             		{
                                        isCall=false;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - DATA -> NEW_LINE (Transition : \\n )");
                                        yybegin(NEW_LINE);
                                    }  
<DATA>			.					{}

/************************/
/* FUNC STATE           */
/************************/
<FUNC>			{COMMENT_LINE}	{}
<FUNC>			{STRING}		{}
<FUNC>			{VAR}			{String var = yytext();
								 if(variables.containsKey(var)) variables.put(var, true);}
<FUNC>			\(				{par++;}
<FUNC>			\)				{
                                    par--;
                                    if(par==0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - FUNC -> LINE (Transition : [)] )");
                                        yybegin(LINE);
                                    }
                                }
<FUNC>			[^] 			{}

/************************/
/* ARRAY STATE           */
/************************/
<ARRAY>			{COMMENT_LINE}	{}
<ARRAY>			{STRING}		{}
<ARRAY>			{VAR}			{String var = yytext();
								 Boolean init = variables.get(var);
								 if(init != null && !init){
								    LOGGER.fine("Setting error line "+yyline+1+" for the variable "+var+"."); 
								    setError(location,"The variable " + var + " is used before being initialized. ", yyline+1); 
							     }
							    }
<ARRAY>			\(				{par++;}
<ARRAY>			\)				{
                                    par--;
                                    if(par==0){
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - ARRAY -> LINE (Transition : [)] )");
                                        yybegin(LINE);
                                    }
                                }
<ARRAY>			[^] 			{}

/************************/
/* VAR_EQ STATE         */
/************************/
<VAR_EQ>		{COMMENT_LINE}	{}
<VAR_EQ>		{STRING}		{}
<VAR_EQ>	    {SEE_FUNC}		{}
<VAR_EQ>		{VAR_PAR}		{
                                    String var = yytext().substring(0,yytext().length()-1).trim();
                                    if (!dimension.contains(var)) {
                                        par = 1;
                                        LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR_EQ -> FUNC (Transition : VAR_PAR \""+yytext()+"\" )");
                                        yybegin(FUNC);
                                    }
                                }
<VAR_EQ>		{VAR}			{variable = yytext(); initialized = false;}
<VAR_EQ>		{EQUAL} 		{initialized = true;
								 variables.put(variable, true ); }
<VAR_EQ>		{COMP}          {}
<VAR_EQ>		{SPACE}			{}
<VAR_EQ>		{NUM}|{POINT}	{}
<VAR_EQ>		\n				{if(!initialized) {
									Boolean init = variables.get(variable);
									if(init != null && !init){
									   LOGGER.fine("Setting error line "+yyline+1+" for the variable "+variable+"."); 
									   setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1);
								   }
								 } 
								 initialized = false;
								 LOGGER.fine("["+this.parsedFileName+":"+(yyline+1)+":"+yycolumn+"] - VAR_EQ -> NEW_LINE (Transition : \\n )");
								 yybegin(NEW_LINE);}
<VAR_EQ>		.				{if(!initialized) {
									Boolean init = variables.get(variable);
									if(init != null && !init){
									   LOGGER.fine("Setting error line "+yyline+1+" for the variable "+variable+".");
									   setError(location,"The variable " + variable + " is used before being initialized. ", yyline+1);
								    }
								 } 
								 initialized = false; variable = yytext();
								}

/******************************************************************************/
/* An error is thrown if nothing is catch before this line. 				  */
/******************************************************************************/
/************************/
/* ERROR STATE	        */
/************************/
				[^]            {
									String parsedWord = "Word ["+yytext()+"], code  [" + toASCII(yytext()) + "]";
				                    final String errorMessage = "Analysis failure : Your file could not be analyzed. Please verify that it was encoded in an UNIX format.";
				                    throw new JFlexException(this.getClass().getName(), parsedFileName,
				                                    errorMessage, parsedWord, yyline, yycolumn);
                               }