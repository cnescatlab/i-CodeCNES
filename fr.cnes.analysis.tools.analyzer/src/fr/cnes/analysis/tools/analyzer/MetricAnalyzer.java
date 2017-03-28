/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * This class is used to instantiate a new metric analyzer.
 */
public class MetricAnalyzer extends AbstractAnalyzer {
    /** Logger. */
    private static final Logger LOGGER = Logger.getLogger(MetricAnalyzer.class
            .getName());

    /** List of values found during analysis. **/
    private List<FileValue> values;

    /**
     * Constructor that set the job with string name, extension id and a list
     * of {@link org.eclipse.core.runtime.IPath}.
     * 
     * @param name
     *            the name of this Job
     * @param pFiles
     *            the files to analyze
     * @param pExtensionId
     *            the id of rule/metric contribution
     */
    public MetricAnalyzer(final String name, final List<IPath> pFiles,
            final String pExtensionId) {
        super(name, pFiles, pExtensionId);
        this.values = new LinkedList<FileValue>();
    }

    /**
     * Retrieve the values of analysis.
     * 
     * @return values of the metric analysis
     */
    public List<FileValue> getValues() {
        return this.values;
    }

    /**
     * Set the values with a list.
     * 
     * @param pValues
     *            the list of values to set
     */
    public void setValues(final List<FileValue> pValues) {
        this.values = pValues;
    }

    /**
     * Set the values with an array.
     * 
     * @param pValues
     *            the array of values to set
     */
    public void setValues(final FileValue[] pValues) {
        this.values = new LinkedList<FileValue>();
        for (final FileValue value : pValues) {
            this.values.add(value);
        }
    }

    /*
     * (non-Javadoc)
     * @see
     * fr.cnes.analysis.tools.analyzer.AbstractAnalyzer#runEvaluation(org.eclipse
     * .core.runtime.IConfigurationElement, java.util.List,
     * org.eclipse.core.runtime.IProgressMonitor)
     */
    @Override
    protected IStatus runEvaluation(final IConfigurationElement contribution,
            final List<IPath> pFiles, final IProgressMonitor monitor)
            throws CloneNotSupportedException, CoreException,
            IOException, JFlexException {
        LOGGER.finest("Begin runEvaluation method");

        // Instantiate return variable
        IStatus status = Status.OK_STATUS;

        // Run analysis on all files
        for (final IPath file : pFiles) {

            // Get the evaluation
            final AbstractMetric metric =
                    (AbstractMetric) contribution
                            .createExecutableExtension("class");
            metric.setContribution(contribution);

            // Run the evaluation
            LOGGER.finest("File : " + file.toFile().getName());
            monitor.subTask("Analyzing " + contribution.getAttribute("id")
                    + " on file " + file.toFile().getName());
            this.values.add(this.runMetricOnFile(metric, file));
            monitor.worked(1);

            // Stop analysis if cancel button selected
            if (monitor.isCanceled()) {
                status = Status.CANCEL_STATUS;
                break;
            }
        }

        LOGGER.finest("End runEvaluation method");
        return status;
    }

    /**
     * Compute the provided metric on the file.
     * 
     * @param metric
     *            the evaluation to compute
     * @param file
     *            the file to measure
     * @return list of evaluation (metric value or rule violations)
     * @throws IOException
     *             internal error
     * @throws JFlexException
     *             JFlex analysis error
     */
    private FileValue runMetricOnFile(final AbstractMetric metric,
            final IPath file) throws IOException,
            JFlexException {
        LOGGER.finest("Begin runEvaluationOnFile method");

        // Initializing file reader in the metric
        metric.setInputFile(file);

        LOGGER.finest("End runEvaluationOnFile method");
        return metric.run();
    }

    /*
     * (non-Javadoc)
     * @see org.eclipse.core.runtime.jobs.Job#canceling()
     */
    @Override
    protected void canceling() {
        this.values.clear();
    }
}
