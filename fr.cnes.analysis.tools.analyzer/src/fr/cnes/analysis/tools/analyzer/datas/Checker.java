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
import java.util.LinkedList;
import java.util.List;

public class Checker {

    private String name;
    private String id;
    private String langageId;

    /** Check results list. */
    private List<CheckResult> checkResults = new LinkedList<CheckResult>();;

    /**
     * Method to add a violation, knowing its location and line.
     * 
     * @param pLocation
     *            the location
     * 
     * @param pMessage
     *            violation's message
     * @param pLine
     *            the line
     * @throws JFlexException
     *             exception thrown when cloning error appears
     */
    protected void setError(final String pLocation, final String pMessage, final int pLine)
            throws JFlexException {
        CheckResult checkResult = new CheckResult(name, id, langageId);
        checkResult.setLine(pLine);
        checkResult.setLocation(pLocation);
        checkResult.setMessage(pMessage);
        checkResults.add(checkResult);
    }

    /**
     * 
     * @param file
     * @throws FileNotFoundException
     */

    public void setInputFile(final File file) throws FileNotFoundException {
        this.checkResults = new LinkedList<CheckResult>();
    }
}
