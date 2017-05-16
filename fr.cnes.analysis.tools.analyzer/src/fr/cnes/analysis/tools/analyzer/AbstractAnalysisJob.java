/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 


package fr.cnes.analysis.tools.analyzer;

import java.io.File;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;

/**
 * This class is used run analysis on one analyzer.
 */
public abstract class AbstractAnalysisJob extends Job {
    /** Extension point id. **/
    public static final String ANALYSIS_EXT_ID =
            "fr.cnes.analysis.tools.analyzer";
    /** Logger. **/
    public static final Logger LOGGER = Logger
            .getLogger(AbstractAnalysisJob.class.getName());

    /** Extension id for the analyzer to use. **/
    private String extensionId;
    /** List of files to analyze. **/
    private List<File> files;

    /**
     * Constructor for this Job with extension id in parameter and files' path.
     * 
     * @param pExtensionId
     *            the extension id of the analyzer on which this job applies
     * @param pFiles
     *            list of path for files to analyze
     */
    public AbstractAnalysisJob(final String pExtensionId,
            final List<File> pFiles) {
        super("Running analysis...");
        this.extensionId = pExtensionId;
        this.files = pFiles;
    }

    /**
     * Getter for the extension id.
     * 
     * @return the extensionId
     */
    public String getExtensionId() {
        return this.extensionId;
    }

    /**
     * Getter for the files
     * 
     * @return the files
     */
    public List<File> getFiles() {
        return this.files;
    }

    /**
     * Setter for the extension id.
     * 
     * @param pExtensionId
     *            the extensionId to set
     */
    public void setExtensionId(final String pExtensionId) {
        this.extensionId = pExtensionId;
    }

    /**
     * Setter for the files
     * 
     * @param pFiles
     *            the files to set
     */
    public void setFiles(final List<File> pFiles) {
        this.files = pFiles;
    }

    /**
     * This method returns an analyzer contribution giving analyzer extension
     * point contribution id. If no analyzer corresponds to the id, the method
     * returns null.
     * 
     * @param analyzerId
     *            the analyzer id
     * @return the analyzer contribution
     * @throws NullContributionException
     *             when there is no contribution associated to this id
     */
    public static IConfigurationElement
            getContribution(final String analyzerId)
                    throws NullContributionException {
        LOGGER.finest("Begin getContribution method");

        // Get all contributions
        final IConfigurationElement[] contributions =
                Platform.getExtensionRegistry().getConfigurationElementsFor(
                        ANALYSIS_EXT_ID);

        // Look for the corresponding one
        IConfigurationElement analyzeContrib = null;
        int index = 0;
        while ((analyzeContrib == null) && index < contributions.length) {
            if (contributions[index].getAttribute("id").equals(analyzerId)) {
                analyzeContrib = contributions[index];
            }
            index = index + 1;
        }

        if (analyzeContrib == null) {
            throw new NullContributionException("No contribution found for "
                    + analyzerId);
        } else {
            LOGGER.finest("End getContribution method");
            return analyzeContrib;
        }

    }
}
