/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;

/**
 * This class is used run analysis on a rule analyzer.
 */
public class RuleAnalysisJob extends AbstractAnalysisJob {
    /** Array of descriptors used to store result of analysis. **/
    private List<Violation> violations;

    /**
     * Constructor for this Job with extension id in parameter and files' path.
     * 
     * @param pExtensionId
     *            the extension id of the analyzer on which this job applies
     * @param pFiles
     *            list of path for files to analyze
     */
    public RuleAnalysisJob(final String pExtensionId, final List<IPath> pFiles) {
        super(pExtensionId, pFiles);
        this.violations = new LinkedList<Violation>();
    }

    /**
     * Getter for the violations.
     * 
     * @return the violations
     */
    public List<Violation> getViolations() {
        return this.violations;
    }

    /**
     * Setter for the violations.
     * 
     * @param pViolations
     *            the violations to set
     */
    public void setViolations(final List<Violation> pViolations) {
        this.violations = pViolations;
    }

    /*
     * (non-Javadoc)
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public IStatus run(final IProgressMonitor monitor) {
        LOGGER.finest("Begin run method");

        // Instantiate return variable
        IStatus status = Status.OK_STATUS;

        try {
            final IConfigurationElement contribution =
                    AbstractAnalysisJob.getContribution(this.getExtensionId());
            if (contribution == null) {
                throw new CoreException(new Status(IStatus.ERROR,
                        "AnalysisJob", 0, "No contribution for analyzer id : "
                                + this.getExtensionId(), null));
            } else {
                final RuleAnalyzer analyzer =
                        new RuleAnalyzer("Run "
                                + contribution.getAttribute("name")
                                + " computation...", this.getFiles(),
                                contribution.getAttribute("extensionId"));

                analyzer.setUser(true);

                status = analyzer.run(monitor);
                this.violations = analyzer.getViolations();

            }
        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            Status.ERROR, exception.getMessage(), exception);
        } catch (final NullContributionException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            Status.ERROR, exception.getMessage(), exception);
        }
        LOGGER.finest("End run method");
        return status;
    }
}
