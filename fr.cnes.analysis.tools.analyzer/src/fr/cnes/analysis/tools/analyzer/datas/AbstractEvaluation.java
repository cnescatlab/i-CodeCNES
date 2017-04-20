/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.FileNotFoundException;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;

/**
 * Abstract class implementing the generic application of a rule or metric over
 * a file. For each rule (or metric) and file, an instance is made.
 * 
 */
public abstract class AbstractEvaluation {

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
     * Set the input file to be analyzed.
     * 
     * @param file
     *            file's path
     * @throws FileNotFoundException
     *             exception thrown when a file is not found
     */
    public abstract void setInputFile(IPath file) throws FileNotFoundException;
}
