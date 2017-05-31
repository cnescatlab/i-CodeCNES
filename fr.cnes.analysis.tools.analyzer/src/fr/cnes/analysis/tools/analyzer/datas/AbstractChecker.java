/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.datas;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;

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
 * the file currently analyzed. To do so, use {@link #getCheckResults(File)}
 * getter.
 * </p>
 */
public abstract class AbstractChecker {
    /** Analysed file. */
    private File inputFile;

    /** List of {@link CheckResult} found during analysis. **/
    private List<CheckResult> checkResults = new ArrayList<>();

    private CheckResult checkResult;// TODO à voir pour l'enlever

    /**
     * Run analysis for considering file and rule.
     * 
     * @return list of {@link CheckResult}s found during analysis
     * @throws IOException
     *             IO problem occurred
     * @throws JFlexException
     *             JFlex error during analysis
     */
    public abstract List<CheckResult> run() throws IOException, JFlexException;

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
     * @throws JFlexException
     *             exception thrown when cloning error appears
     */
    protected void setError(final String pLocation, final String pMessage, final int pLine)
            throws JFlexException {
        // TODO Improve how is set the language id
        CheckResult checkResult = new CheckResult(this.getContribution().getAttribute("name"),
                this.getContribution().getAttribute("id"),
                this.getContribution().getDeclaringExtension().getExtensionPointUniqueIdentifier());
        checkResult.setLine(pLine);
        checkResult.setLocation(pLocation);
        checkResult.setMessage(pMessage);
        checkResult.setFile(inputFile);
        this.checkResults.add(checkResult);

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
     * @throws JFlexException
     *             exception thrown when cloning error appears
     */
    protected void computeMetric(final String pLocation, final float pValue, final int pLine)
            throws JFlexException {

        // TODO Improve how is set the language id
        CheckResult checkResult = new CheckResult(this.getContribution().getAttribute("name"),
                this.getContribution().getAttribute("id"),
                this.getContribution().getDeclaringExtension().getExtensionPointUniqueIdentifier());
        checkResult.setLine(pLine);
        checkResult.setLocation(pLocation);
        checkResult.setValue(pValue);
        checkResult.setFile(this.inputFile);
        this.checkResults.add(checkResult);

    }

    /**
     * Set the input file to be analyzed.
     * 
     * @param inputFile
     *            file's path
     * @throws FileNotFoundException
     *             exception thrown when a file is not found
     */
    public void setInputFile(final File pInputFile) throws FileNotFoundException {
        this.checkResults = new LinkedList<CheckResult>();
        // TODO improve language identifier retrieval
        this.checkResult = new CheckResult(this.getContribution().getAttribute("name"),
                this.getContribution().getAttribute("id"),
                this.getContribution().getDeclaringExtension().getExtensionPointUniqueIdentifier());
        this.checkResult.setFile(pInputFile);
        this.inputFile = pInputFile;
    }

    /**
     * @return the checkResult
     */
    public CheckResult getCheckResult() {
        return checkResult;
    }

    /**
     * @param pCheckResult
     *            the checkResult to set
     */
    public void setCheckResult(CheckResult pCheckResult) {
        this.checkResult = pCheckResult;
    }

    /** The configuration element linked to this evaluation. **/
    private IConfigurationElement contribution;

    /**
     * Getter for the contribution.
     * 
     * @return the contribution
     */
    public IConfigurationElement getContribution() {
        return this.contribution;
    }

    /**
     * Setter for the contribution.
     * 
     * @param pContribution
     *            the new contribution
     */
    public void setContribution(final IConfigurationElement pContribution) {
        this.contribution = pContribution;
    }

    /**
     * Getter for the list of {@link CheckResult}.
     * 
     * @return the {@link CheckResult}s
     */
    public List<CheckResult> getCheckResults() {
        return this.checkResults;
    }

    /**
     * This method retrieves from {@link #checkResults} all {@link CheckResult}
     * items that have the file in parameter as {@link CheckResult#getFile()}
     * attribute.
     * 
     * @param file
     *            The files to restrict the {@link CheckResult} to return.
     * @return only {@link CheckResult}s that are set for the same file as file
     *         parameter.
     */
    public List<CheckResult> getCheckResults(File file) {
        List<CheckResult> fileCheck = new ArrayList<>();
        for (CheckResult check : this.checkResults) {
            if (check.getFile() != null && check.getFile().equals(file)) {
                fileCheck.add(check);
            }
        }
        return fileCheck;
    }

    /**
     * Setter for the list of {@link CheckResult}s.
     * 
     * @param pCheckResults
     *            the {@link CheckResult}s to set
     */
    public void setCheckResults(final List<CheckResult> pCheckResults) {
        this.checkResults = pCheckResults;
    }

    /**
     * Getter for the {@link #inputFile} analyzed
     * 
     * @return the analyzed file.
     */
    public File getInputFile() {
        return inputFile;
    }

}
