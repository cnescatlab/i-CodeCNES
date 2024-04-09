/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data;

import java.util.List;

/**
 * This rule is intended to add record parsing exceptions.
 *  
 */
public class ParsingError extends AbstractChecker {
	
	public final static String PARSING_ERROR_ID = "fr.cnes.icode.parsingError";
	public final static String PARSING_ERROR_LANGUAGE = "fr.cnes.icode.parsingError";
	public final static String PARSING_ERROR_NAME = "Parsing error";
	
	public ParsingError() {
	}
	

	@Override
	public List<CheckResult> run() {
		return getCheckResults();
	}
}
