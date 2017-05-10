/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.NAME.Homonomy rule.		  */
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
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMNAMEHomonymy
%extends AbstractRule
%public
%line

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, LOCAL, FUNCTIONSTATE, FUNCOMMENT, BEGINFUNC, DECLARESTATE, FUNCDECLARESTATE

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z0-9\_\-]+
STRING		 = \'[^\']*\' | \"[^\"]*\"
OPTCHAR		 = \# | \! | % | \* | @ | \^ | \' | , | \/ | : | = | \+ | \? | \[ | \]
VARCOMP		 = {OPTCHAR} | (\$)?{VAR} | \$\{ {VAR} \}
EXTENDEDVAR	 = \$\{ {VARCOMP}+ \}
OPTION		 = "-"[a-z]

LOCAL		 = "local"
DECLARE		 = "declare" {SPACE}+ {OPTION}*

FUNCSTART	 = \{ | \( | \(\( | \[\[ | "if" | "case" | "select" | "for" | "while" | "until"
FUNCEND		 = \} | \) | \)\) | \]\] | "fi" | "esac" | "done"

																
%{
    /* MAINPROGRAM: constant for main program localisation */
    private static final String MAINPROGRAM = "MAIN PROGRAM";
    /* ERROR_ON_VARIABLE: constant for variable error message */
    private static final String ERROR_ON_VARIABLE = " -> Error with the variable named ";
    /* LOCAL_VAR_SAME_NAME: constant for local variable message */
    private static final String LOCAL_VAR_SAME_NAME = ". There is a local variable with the same name.";
    /* FUNCT_SAME_NAME: constant for function name message */
    private static final String FUNCT_SAME_NAME = ". There is a function with the same name.";
    /* location: the current function name, or main program, that is the initial value */
    private String location = MAINPROGRAM;
    /* functions: the list of function names in the code analyzed so far */
    private List<String> functions = new ArrayList<String>();
    /* localVariables: the list of local variables in the code analyzed so far */
    private List<String> localVariables = new ArrayList<String>();
    /* globalVariables: the list of global variables in the code analyzed so far */
    private List<String> globalVariables = new ArrayList<String>();
    /* currentLocals: the list of local variables in the current function */
    private List<String> currentLocals = new ArrayList<String>();
    /* extraGlobals: the list of the local variables of an encapsulating function */
    private List<String> extraGlobals = new ArrayList<String>();
    
    /* currentOpening: the opening type of the current function */
    private String currentOpening = "";
    /* nbOpenings: the nuber of times the current opening has been used in the function number */ 
    private int nbOpenings = 0;
    /* doneOpenings: a list containing the "done" type family of openings */
    private final List<String> doneOpenings = new ArrayList<String>(Arrays.asList("select", "for", "while", "until"));
    
    /* FunctionInfo: structure recording current function information */
    /* Only used in this class */
    private static class FunctionInfo {
        /* name: function name (localisation) */
        private String name; 
        /* openingType: function opening type */
        private String openingType;  
        /* nbOpenings: number of current openings used */
        private int    nbOpenings;
        /* parentCurrentLocals: list of current locals of the encapsulating function */
        /* It is kept to be able to restore the current locals values in the encapsulating */
        /* function when the current function ends and we go back up a level */
        private List<String>  parentCurrentLocals;
        /* parentExtraGlobals: list of extra globals: the locals of the encapsulating function */
        /* It is kept to be able to restore the extra globals values in the encapsulating */
        /* function when the current function ends and we go back up a level */
        private List<String>  parentExtraGlobals;
        
        /* Class constructors */
        public FunctionInfo() {}
        public FunctionInfo(final String namePar, final String openingTypePar, 
                            final int nbOpeningsPar, final List<String> parentCurrentLocalsPar, 
                            final List<String> parentExtraGlobalsPar) {
            name = namePar;
            openingType = openingTypePar;
            nbOpenings = nbOpeningsPar;
            parentCurrentLocals = new ArrayList<String>(parentCurrentLocalsPar);
            parentExtraGlobals = new ArrayList<String>();
            parentExtraGlobals.addAll(parentExtraGlobalsPar);
        }
        
        /* Class member accessors */
        /* Only the getters are defined: the values are set by the constructor */
        public String getName() { return this.name; }
        public String getOpeningType() { return this.openingType; }
        public int getNbOpenings() { return this.nbOpenings; }
        public List<String> getParentCurrentLocals() { return this.parentCurrentLocals; }
        public List<String> getParentExtraGlobals() { return this.parentExtraGlobals; }
        
    }
    
    /* currentFuncInfo: the function information for the current function */ 
    private FunctionInfo currentFuncInfo = null;
    
    /* Pile to stack up function information when there are nested functions */
    private final Stack<FunctionInfo> functionPile = new Stack<FunctionInfo>();
    
    public COMNAMEHomonymy() {
        
    }
    
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        super.setInputFile(file);
        this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
    }

    /** 
      * checkVar: adds the current variable name (var) to the list of global variables  
      * and checks for violations. Called from YYINITIAL.
      */
    private void checkVar(final String var) throws JFlexException {
        if(functions.contains(var)) {
            setError(location, ERROR_ON_VARIABLE + var + FUNCT_SAME_NAME, yyline+1);
        }
        if(location.equals(MAINPROGRAM)) {
            globalVariables.add(var);
            if(localVariables.contains(var)) {
                setError(location, ERROR_ON_VARIABLE + var + LOCAL_VAR_SAME_NAME, yyline+1);
            }
        }
    }    

    /** 
      * checkVarFunc: adds the current variable name (var) to the list of global variables
      * and checks for violations. Called from FUNCTIONSTATE, so when within a function
      */
    private void checkVarFunc(final String var) throws JFlexException {
        if(functions.contains(var)) {
            setError(location, ERROR_ON_VARIABLE + var + FUNCT_SAME_NAME, yyline+1);
        }
        if(! globalVariables.contains(var)) {
            if(! currentLocals.contains(var)) {
                globalVariables.add(var);
                if (localVariables.contains(var)) {
                    setError(location, ERROR_ON_VARIABLE + var + LOCAL_VAR_SAME_NAME, yyline+1);
                }
            }
        }
    }

    /** 
      * isSameFamily: checks if 2 opening values are in the same opening mode family
      */
    private Boolean isSameFamily(final String opening1, final String opening2) throws JFlexException {
        return doneOpenings.contains(opening1) && doneOpenings.contains(opening2);
    }    
    
    /** 
      * isCurrentOpening: checks if the opening value (opening) is the current function opening 
      * mode or belongs to the same family of openings
      */
    private Boolean isCurrentOpening(final String opening) throws JFlexException {
        return opening.equals(currentOpening) || isSameFamily(opening, currentOpening); 
    }    

    /** 
      * isCurrentClosing: checks if the closing value is the current function closing mode
      */
    private Boolean isCurrentClosing(final String closing) throws JFlexException {
        Boolean returnValue = false;
        switch (closing) {
            case "}": 
                returnValue = currentOpening.equals("{");
                break;
            case ")":
                returnValue = currentOpening.equals("(");
                break;
            case "))":
                returnValue = currentOpening.equals("((");
                break;
            case "]]":
                returnValue = currentOpening.equals("[[");
                break;
            case "fi":
                returnValue = currentOpening.equals("if");
                break;
            case "esac":
                returnValue = currentOpening.equals("case");
                break;
            case "done":
                returnValue = doneOpenings.contains(currentOpening);
                break;
            default :
                returnValue = false;
        } 
        return returnValue;
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
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}
		
/************************/
/* FUNCOMMENT STATE	    */
/************************/
<FUNCOMMENT>   	
		{
				\n             	{yybegin(FUNCTIONSTATE);}  
			   	.              	{}
		}
		
		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{location = yytext(); functions.add(yytext()); yybegin(BEGINFUNC);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}

/************************/
/* BEGINFUNC STATE	    */
/************************/
<BEGINFUNC>   	
		{
				\(\)			{}
				{FUNCSTART}		{currentOpening = yytext(); 
								 nbOpenings=1; 
								 currentFuncInfo = new FunctionInfo(location, currentOpening, nbOpenings, currentLocals, extraGlobals);
								 extraGlobals.addAll(currentLocals);
								 currentLocals.clear();
								 yybegin(FUNCTIONSTATE);}
			   	.              	{}
		}
		
		
/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();
								 functions.add(location);  
								 yybegin(BEGINFUNC);}
			    {VAR}\=			{String var = yytext().substring(0, yytext().length()-1); checkVar(var);}
				{DECLARE}		{yybegin(DECLARESTATE);}
				{STRING}		{}
	      		. | \n         	{}
		}

/************************/
/* DECLARESTATE STATE		    */
/************************/
<DECLARESTATE>   	
		{
				{VAR}			{String var = yytext();checkVar(var); yybegin(YYINITIAL);}
				\n             	{yybegin(YYINITIAL);}  
			   	.              	{}
		}		

/************************/
/* FUNCDECLARESTATE STATE		    */
/************************/
<FUNCDECLARESTATE>   	
		{
				{VAR}			{String var = yytext().substring(0, yytext().length()-1); checkVarFunc(var); yybegin(FUNCTIONSTATE);}
				\n             	{yybegin(FUNCTIONSTATE);}  
			   	.              	{}
		}		

		/************************/
/* LOCAL STATE		    */
/************************/
<LOCAL>   	
		{
				{VAR}			{localVariables.add(yytext()); 
								 currentLocals.add(yytext()); 
								 if(functions.contains(yytext())) setError(location,ERROR_ON_VARIABLE + yytext() + FUNCT_SAME_NAME, yyline+1);
								 if(globalVariables.contains(yytext())) setError(location,ERROR_ON_VARIABLE + yytext() + ". There is a global variable with the same name.", yyline+1);						 
								 if(extraGlobals.contains(yytext())) setError(location,ERROR_ON_VARIABLE + yytext() + ". There is a local variable of encapsulation function with the same name.", yyline+1);						 
								 yybegin(FUNCTIONSTATE);}
				{STRING}		{}
				\n             	{yybegin(FUNCTIONSTATE);}  
			   	.              	{}
		}

/************************/
/* FUNCTIONSTATE STATE	*/
/************************/
<FUNCTIONSTATE>   	
		{
				{COMMENT_WORD}	{yybegin(FUNCOMMENT);}
				{STRING}		{}
				{EXTENDEDVAR}		{}
				{LOCAL}			{yybegin(LOCAL);}
				{VAR}\=			{String var = yytext().substring(0, yytext().length()-1); checkVarFunc(var);}
				{DECLARE}		{yybegin(FUNCDECLARESTATE);}
				{FUNCSTART}		{if(isCurrentOpening(yytext())) nbOpenings++;}
				{FUNCEND}		{if(isCurrentClosing(yytext())) nbOpenings--;
								 if(nbOpenings==0) {
									 if (!functionPile.empty()) {
										/* Reload higher level function information */ 
										currentFuncInfo = functionPile.pop();
										location = currentFuncInfo.getName();
										currentOpening = currentFuncInfo.getOpeningType();
										nbOpenings = currentFuncInfo.getNbOpenings();
										currentLocals = currentFuncInfo.getParentCurrentLocals();
										extraGlobals = currentFuncInfo.getParentExtraGlobals();
									 } else {						 
									    /* We are back at main program level */
										location = MAINPROGRAM;
										currentOpening = "";
										nbOpenings = 0;
										currentLocals.clear();
										extraGlobals.clear();
										yybegin(YYINITIAL);
									}
								 }
				}
				/* functions within a function */
				{FUNCTION}     	{functionPile.push(currentFuncInfo);
								 yybegin(NAMING);}
				{FUNCT}			{location = yytext().substring(0,yytext().length()-2).trim();
								 functions.add(location);
								 functionPile.push(currentFuncInfo);
								 yybegin(BEGINFUNC);}
			   	.              	{}
		}

/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}