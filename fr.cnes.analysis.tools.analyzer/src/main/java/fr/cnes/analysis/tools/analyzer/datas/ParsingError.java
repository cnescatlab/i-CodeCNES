package fr.cnes.analysis.tools.analyzer.datas;

import java.io.IOException;
import java.util.List;

/**
 * This rule is intended to add record parsing exceptions.
 *  
 */
public class ParsingError extends AbstractChecker {
	
	public final static String PARSING_ERROR_ID = "fr.cnes.analysis.tools.analyzer.parsingError";
	public final static String PARSING_ERROR_LANGUAGE = "fr.cnes.analysis.tools.analyzer.parsingError";
	public final static String PARSING_ERROR_NAME = "Parsing error";
	
	public ParsingError() {
	}
	

	@Override
	public List<CheckResult> run() throws IOException {
		return getCheckResults();
	}
}
