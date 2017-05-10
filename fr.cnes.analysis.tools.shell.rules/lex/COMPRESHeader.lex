/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

/**********************************************************************************/
/* This file is used to generate a rule checker for COM.PRES.Header rule.		  */
/* For further information on this, we advise you to refer to RNC manuals.	      */
/* As many comments have been done on the ExampleRule.lex file, this file         */
/* will restrain its comments on modifications.								      */
/*																			      */
/**********************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

%%

%class COMPRESHeader
%extends AbstractRule
%public
%line
%ignorecase

%function run
%yylexthrow JFlexException
%type List<Violation>


%state COMMENT, NAMING, AVOID

COMMENT_WORD = \#
FUNCTION     = "function"
FUNCT		 = {VAR}{SPACE}*\(\)
SPACE		 = [\ \r\t\f]
VAR		     = [a-zA-Z][a-zA-Z0-9\_]*		| \$\#
STRING		 = \'[^\']*\' | \"[^\"]*\"

																
%{
	String location = "MAIN PROGRAM";
	List<String> linesType = new LinkedList<String>();
	List<StringBuilder> locations = new LinkedList<StringBuilder>();
	List<Integer> lines = new LinkedList<Integer>();
	boolean first = true;
	int errorLine = 0, brackets = 0;

    public COMPRESHeader() {

    }
	
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(new Path(file.getAbsolutePath()).toOSString());
	}
	
	private void addType(String type, String location, int line){
		if (linesType.isEmpty()){
			linesType.add(type);
			StringBuilder buffer = new StringBuilder();
			buffer.append(location);
			locations.add(buffer);
			lines.add(line);
		} else {
			int last = linesType.size()-1;
			if (linesType.get(last).equals("comment") && type.equals("comment")){
				locations.get(last).append(location);
			} else {
				linesType.add(type);
				StringBuilder buffer = new StringBuilder();
				buffer.append(location);
				locations.add(buffer);
				lines.add(line);
			}
		}
	}
	
	/**
	 * Throw the errors
	 * 
	 * @throws JFlexException
	 */
	private void raiseErrors() throws JFlexException {
		if(!linesType.get(0).equals("comment")) 
			setError(locations.get(0).toString(),"The file should start with a brief description.", lines.get(0));
		int max = linesType.size();
		for (int index = 0; index < max; index++) {
			String position = linesType.get(index);
			if (position.equals("function")) {
				/** Check limits **/
				if(index >= 1 || index < max) {
					/** If there is no comment before or after -> error**/ 
					if (!linesType.get(index-1).equals("comment") &&
						!linesType.get(index+1).equals("comment")) {
						setError(locations.get(index).toString(),"The function should have a header with a brief description.", lines.get(index));
					}
				}
			}
		}
    }
    	
%}

%eofval{
	raiseErrors();
	linesType.clear();
	locations.clear();
	lines.clear();
	
	return getViolations();
%eofval}


%%          



/************************/



/************************/
/* COMMENT STATE	    */
/************************/
<COMMENT>   	
		{
				\n             	{this.addType("comment", location, yyline + 1); yybegin(YYINITIAL);}  
			   	.              	{location = location + yytext();}
		}
		

/************************/
/* AVOID STATE	    	*/
/************************/
<AVOID>			\n|\r			{yybegin(YYINITIAL);}
<AVOID>			.				{}

		
/************************/
/* NAMING STATE	    */
/************************/
<NAMING>   	
		{
				{VAR}			{if (first){
									errorLine = yyline + 1;
									location = yytext();
									first=false;
								 }
								}
				\n             	{this.addType("function", location, errorLine);
								 first = true;
								 yybegin(YYINITIAL);
								}  
			   	.              	{}
		}

/************************/
/* YYINITIAL STATE	    */
/************************/
<YYINITIAL>
		{
			  	{COMMENT_WORD} 	{location = " "; yybegin(COMMENT);}
				{FUNCTION}     	{yybegin(NAMING);}
				{FUNCT}			{location=yytext().substring(0,yytext().length()-2).trim(); errorLine=yyline+1; yybegin(NAMING);}
			    {STRING}		{}
			    {VAR}			{}
			    \{				{brackets++;}
			    \}				{brackets--;
			    				 if(brackets==0) {
			    				 	this.addType("line", location, yyline + 1);
			    				 	yybegin(AVOID);
								 }
								}
				\n				{this.addType("line", location, yyline + 1);}
	      		.              	{}
		}

/************************/
/* ERROR STATE	        */
/************************/
				.|\n            {}