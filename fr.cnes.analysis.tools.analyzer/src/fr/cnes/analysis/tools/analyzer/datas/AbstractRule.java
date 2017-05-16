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
    /** A violation used to clone over analysis. **/
    private Violation violation;
    /** List of violations found during analysis. **/
    private List<Violation> violations;

    /**
     * Getter for the violation.
     * 
     * @return the violation
     */
    public Violation getViolation() {
        return this.violation;
    }

    /**
     * Getter for the list of violations.
     * 
     * @return the violations
     */
    public List<Violation> getViolations() {
        return this.violations;
    }

    /**
     * Setter for the violation.
     * 
     * @param pViolation
     *            the violation to set
     */
    public void setViolation(final Violation pViolation) {
        this.violation = pViolation;
    }

    /**
     * Setter for the list of violations.
     * 
     * @param pViolations
     *            the violations to set
     */
    public void setViolations(final List<Violation> pViolations) {
        this.violations = pViolations;
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
    public abstract List<Violation> run() throws IOException, JFlexException;

    /**
     * Method to add a violation, knowing its location and line.
     * 
     * @param pLocation
     *            the location
     *            
     * @param pMessage
     * 			  violation's message
     * @param pLine
     *            the line
     * @throws JFlexException
     *             exception thrown when cloning error appears
     */
    protected void setError(final String pLocation, final String pMessage, final int pLine)
            throws JFlexException {
        try {
            this.violation.setLine(pLine);
            this.violation.setLocation(pLocation);
            this.violation.setMessage(pMessage);
            this.violations.add(this.violation.clone());
        } catch (final CloneNotSupportedException exception) {
            throw new JFlexException(exception);
        }
    }

    /*
     * (non-Javadoc)
     * @see
     * fr.cnes.analysis.tools.analyzer.datas.AbstractEvaluation#setInputFile
     * (org.eclipse.core.runtime.IPath)
     */
    @Override
    public void setInputFile(final File file) throws FileNotFoundException {
        this.violations = new LinkedList<Violation>();
        this.violation =
                new Violation(this.getContribution().getAttribute("name"),
                        this.getContribution().getAttribute("id"), file);
    }
}
