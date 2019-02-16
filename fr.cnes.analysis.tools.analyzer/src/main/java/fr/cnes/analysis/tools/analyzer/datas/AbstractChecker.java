/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.datas;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * This class must be extended by any Rule analyzer or Metric computer
 * contributing to i-Code CNES Analyzer.
 * 
 * <p>
 * Once an analysis {@link #run()} it's possible to add all informations of
 * Rule's violation using {@link #setError(String, String, int)} or metric
 * computation using {@link #computeMetric(String, float, int)}.
 * </p>
 * 
 * <p>
 * Sometimes, metric computing requires to retrieves already computed data on
 * the file currently analyzed. To do so, use {@link #getCheckResults()}
 * getter.
 * </p>
 */
public abstract class AbstractChecker implements IChecker {

    /** Class name */
    private static final String CLASS = AbstractChecker.class.getName();
    /** Analyzed file. */
    private File inputFile;

    /** List of {@link CheckResult} found during analysis. **/
    private List<CheckResult> checkResults = new ArrayList<>();

    /**
     * Method to add a {@link CheckResult}, knowing its location and line.
     * 
     * @param pLocation
     *            the location
     * 
     * @param pMessage
     *            result's message
     * @param pLine
     *            the line
     */
    protected void setError(final String pLocation, final String pMessage, final int pLine) {
        final String method = "setError";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pLocation, pMessage, Integer.valueOf(pLine)
        });
        final CheckResult checkResult = new CheckResult("name",
                        "id",
                        "languageId");
        checkResult.setLine(Integer.valueOf(pLine));
        checkResult.setLocation(pLocation);
        checkResult.setMessage(pMessage);
        checkResult.setFile(inputFile);
        this.checkResults.add(checkResult);
        ICodeLogger.exiting(CLASS, method);

    }

    /**
     * Method to add a {@link CheckResult}, knowing its location and line.
     * 
     * @param pLocation
     *            the location
     * 
     * @param pValue
     *            result's message
     * @param pLine
     *            the line
     */
    protected void computeMetric(final String pLocation, final float pValue, final int pLine) {
        final String method = "computeMetric";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pLocation, Float.valueOf(pValue), Integer.valueOf(pLine)
        });
        final CheckResult checkResult = new CheckResult("name", "id","languageId");
        checkResult.setLine(Integer.valueOf(pLine));
        checkResult.setLocation(pLocation);
        checkResult.setValue(Float.valueOf(pValue));
        checkResult.setFile(this.inputFile);
        this.checkResults.add(checkResult);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Set the input file to be analyzed.
     * 
     * @param pInputFile
     *            file's path
     * @throws FileNotFoundException
     *             exception thrown when a file is not found
     */
    @Override
    public void setInputFile(final File pInputFile) throws FileNotFoundException {
        final String method = "setInputFile";
        ICodeLogger.entering(CLASS, method, pInputFile);
        this.checkResults = new LinkedList<CheckResult>();
        final CheckResult checkResult = new CheckResult("name", "id","languageId");
        checkResult.setFile(pInputFile);
        this.inputFile = pInputFile;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the list of {@link CheckResult}.
     * 
     * @return the {@link CheckResult}s
     */
    @Override
    public List<CheckResult> getCheckResults() {
        final String method = "getCheckResults";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.checkResults);
        return this.checkResults;
    }

    /**
     * Setter for the list of {@link CheckResult}s.
     * 
     * @param pCheckResults
     *            the {@link CheckResult}s to set
     */
    @Override
    public void setCheckResults(final List<CheckResult> pCheckResults) {
        final String method = "setCheckResults";
        ICodeLogger.entering(CLASS, method, pCheckResults);
        this.checkResults = pCheckResults;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the {@link #inputFile} analyzed
     * 
     * @return the analyzed file.
     */
    @Override
    public File getInputFile() {
        final String method = "getInputFile";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, inputFile);
        return inputFile;
    }

}
