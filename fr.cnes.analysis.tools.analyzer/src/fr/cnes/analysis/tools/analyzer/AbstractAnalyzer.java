/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 

package fr.cnes.analysis.tools.analyzer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * This class defines the structure of an analyzer. The analyzer is the class
 * that runs all the analysis, for all rules or metrics. Each analyzer, for
 * metric or rule, for each language, must extend this class.
 */
public abstract class AbstractAnalyzer extends Job {
    /** Logger. */
    private static final Logger LOGGER = Logger
            .getLogger(AbstractAnalyzer.class.getName());

    /** Files to be analyzed. */
    private List<File> files;
    /** the id of rule/metric contribution **/
    private String extensionId;

    /**
     * Constructor that set the job with string name, extension id and a list
     * of {@link org.eclipse.core.runtime.IPath}.
     * 
     * @param name
     *            the name of this Job
     * @param pFilePath
     *            the files to analyze
     * @param pExtensionId
     *            the id of rule/metric contribution
     */
    public AbstractAnalyzer(final String name, final List<File> pFilePath,
            final String pExtensionId) {
        super(name);
        this.files = pFilePath;
        this.extensionId = pExtensionId;
    }

    /**
     * Retrieve the files.
     * 
     * @return the files path.
     */
    public List<File> getFiles() {
        return this.files;
    }

    /**
     * Getter for the extension id.
     * 
     * @return the id of rule/metric contribution
     */
    public String getExtensionId() {
        return this.extensionId;
    }

    /**
     * Set the files path.
     * 
     * @param inputFiles
     *            the files path.
     */
    public void setFiles(final List<File> inputFiles) {
        this.files = inputFiles;
    }

    /**
     * Setter for the id of rule/metric contribution.
     * 
     * @param pExtensionId
     *            the extensionId to set
     */
    public void setExtensionId(final String pExtensionId) {
        this.extensionId = pExtensionId;
    }

    /*
     * (non-Javadoc)
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(final IProgressMonitor monitor) {
        LOGGER.finest("Begin run method");

        // Instantiate return variable
        IStatus status = Status.OK_STATUS;

        try {
            // Instantiate other variables
            final IConfigurationElement[] evalContribs =
                    Platform.getExtensionRegistry()
                            .getConfigurationElementsFor(this.extensionId);

            // Calculate total work to do
            final List<File> pFiles = this.getFiles();
            int totalEval = 0;
            for (final IConfigurationElement contribution : evalContribs) {
                if (PlatformUI.getPreferenceStore().getBoolean(
                        contribution.getAttribute("id"))) {
                    totalEval = totalEval + 1;
                }
            }
            final int totalWork = pFiles.size() * totalEval;

            // Begin global analysis
            monitor.beginTask("Running analysis...", totalWork);
            for (final IConfigurationElement contribution : evalContribs) {

                // Run analysis for this evaluation if it is allowed in the
                // preference store
                if (PlatformUI.getPreferenceStore().getBoolean(
                        contribution.getAttribute("id"))) {
                    status = this.runEvaluation(contribution, pFiles, monitor);
                }

                // Stop analysis if cancel button selected
                if (Status.CANCEL_STATUS.equals(status)) {
                    break;
                }
            }

            // End the job
            monitor.done();

        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            exception.getMessage());
        } catch (final FileNotFoundException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            exception.getMessage());
        } catch (final IOException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            exception.getMessage());
        } catch (final JFlexException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            exception.getMessage());
        } catch (final CloneNotSupportedException exception) {
            LOGGER.log(Level.FINER,
                    exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status =
                    new Status(Status.ERROR,
                            "fr.cnes.analysis.tools.fortran.analyzer",
                            exception.getMessage());
        }
        LOGGER.finest("End run method");
        return status;
    }

    /**
     * Compute the provided evaluation on all files.
     * 
     * @param contribution
     *            evaluation contribution
     * @param pFiles
     *            files to analyze
     * @param monitor
     *            the monitor on which Job is run
     * @return status of this job
     * @throws CloneNotSupportedException
     *             when cloning fails
     * @throws CoreException
     *             internal exception
     * @throws IOException
     *             internal exception
     * @throws JFlexException
     *             JFlex analysis exception
     */
    protected abstract IStatus runEvaluation(
            final IConfigurationElement contribution,
            final List<File> pFiles, final IProgressMonitor monitor)
            throws CloneNotSupportedException, CoreException,
            IOException, JFlexException;
}
