/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.datas.ParsingError;
import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.logger.ICodeLogger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * This class is responsible of applying a rule on a file and to return it's
 * results as a Thread by implementing {@link Callable} interface.
 * 
 * @since 3.0
 */
public class CallableChecker implements Callable<List<CheckResult>> {

    /** Class name */
    private static final String CLASS = CallableChecker.class.getName();

    /** The rule to apply */
    private AbstractChecker rule;
    /** The metric to analyze */
    private File file;

    /**
     * Constructor for {@link CallableChecker}.
     * 
     * @param pRule
     *            to apply
     * @param pInputFile
     *            to analyze
     */
    public CallableChecker(final AbstractChecker pRule, final File pInputFile) {
        final String method = "CallableChecker";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pRule, pInputFile
        });
        this.rule = pRule;
        this.file = pInputFile;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Callable#call()
     */
    @Override
    public List<CheckResult> call() throws IOException {
        final String method = "call";
        ICodeLogger.entering(CLASS, method);
        final List<CheckResult> results = new ArrayList<>();
        rule.setInputFile(file);
        try {
			results.addAll(rule.run());
		} catch (JFlexException exception) {
			ICodeLogger.error(exception.getFileName(), exception.getRuleName(), exception.getMessage());
			CheckResult result = new CheckResult(ParsingError.PARSING_ERROR_NAME,ParsingError.PARSING_ERROR_ID, ParsingError.PARSING_ERROR_LANGUAGE);
			result.setLine(exception.getLine());
		    result.setLocation(exception.getRuleName() +"[l"+exception.getLine()+":c"+exception.getColumn()+"]");
		    result.setMessage(exception.getErrorMessage());
		    result.setFile(file);			
			results.add(result);
		}
        ICodeLogger.exiting(CLASS, method, results);
        return results;
    }

}
