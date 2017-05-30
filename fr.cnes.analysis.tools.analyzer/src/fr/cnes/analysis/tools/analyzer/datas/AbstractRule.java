/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * Abstract class implementing the generic application of a rule over a file.
 * For each rule and file, an instance is made.
 * 
 */
public abstract class AbstractRule extends AbstractEvaluation {
	/** Analysed file. */
	private File file;

	/** List of violations found during analysis. **/
	private List<CheckResult> checkResults;

	private CheckResult checkResult;// TODO à voir pour l'enlever

	/**
	 * Getter for the list of violations.
	 * 
	 * @return the violations
	 */
	public List<CheckResult> getCheckResults() {
		return this.checkResults;
	}

	/**
	 * Setter for the list of violations.
	 * 
	 * @param pCheckResults
	 *            the violations to set
	 */
	public void setViolations(final List<CheckResult> pCheckResults) {
		this.checkResults = pCheckResults;
	}

	/**
	 * Run analysis for considering file and rule.
	 * 
	 * @return list of {@link fr.cnes.analysis.tools.analyzer.datas.Violation}
	 *         found during analysis
	 * @throws IOException
	 *             IO problem occurred
	 * @throws JFlexException
	 *             JFlex error during analysis
	 */
	public abstract List<CheckResult> run() throws IOException, JFlexException;

	/**
	 * Method to add a violation, knowing its location and line.
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
	protected void setError(final String pLocation, final String pMessage, final int pLine) throws JFlexException {

		// TODO set the langage id
		CheckResult checkResult = new CheckResult(this.getContribution().getAttribute("name"),
				this.getContribution().getAttribute("id"), "");
		checkResult.setLine(pLine);
		checkResult.setLocation(pLocation);
		checkResult.setMessage(pMessage);
		checkResult.setFile(file);
		this.checkResults.add(checkResult);

	}

	/**
	 * Method to add a violation, knowing its location and line.
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
	protected void setError(final String pLocation, final float pValue, final int pLine) throws JFlexException {

		// TODO set the langage id
		CheckResult checkResult = new CheckResult(this.getContribution().getAttribute("name"),
				this.getContribution().getAttribute("id"), "");
		checkResult.setLine(pLine);
		checkResult.setLocation(pLocation);
		checkResult.setValue(pValue);
		checkResult.setFile(file);
		this.checkResults.add(checkResult);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * fr.cnes.analysis.tools.analyzer.datas.AbstractEvaluation#setInputFile
	 * (org.eclipse.core.runtime.IPath)
	 */
	@Override
	public void setInputFile(final File file) throws FileNotFoundException {
		this.checkResults = new LinkedList<CheckResult>();
		this.checkResult = new CheckResult(this.getContribution().getAttribute("name"),
				this.getContribution().getAttribute("id"), ""); // TODO si on
																// garde,
																// remplir le
																// langage id
		this.checkResult.setFile(file);
		this.file = file;
	}

	/**
	 * @return the checkResult
	 */
	public CheckResult getCheckResult() {
		return checkResult;
	}

	/**
	 * @param checkResult
	 *            the checkResult to set
	 */
	public void setCheckResult(CheckResult checkResult) {
		this.checkResult = checkResult;
	}
}
