/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

/**
 * Job used to converter inputs from analysis to valuable inputs for the
 * MetricView.
 * 
 */
public class MetricConverter extends Job {
    /** Logger. **/
    public static final Logger LOGGER = Logger.getLogger(MetricConverter.class.getName());

    private static final String CONVERT_JOB_MESSAGE = "Converting results...";

    /** The original inputs. **/
    private CheckResult[] inputs;
    /** A value container which has all values of rules. **/
    private MetricDescriptor[] container;

    /**
     * Empty constructor for this Job.
     */
    public MetricConverter() {
        super(CONVERT_JOB_MESSAGE);
        this.inputs = new CheckResult[0];
        this.container = new MetricDescriptor[0];

    }

    /**
     * Constructor for this Job with an array of violations.
     * 
     * @param checkResults
     *            the inputs
     */
    public MetricConverter(final CheckResult[] checkResults) {
        super(CONVERT_JOB_MESSAGE);
        this.inputs = checkResults.clone();
        this.container = new MetricDescriptor[0];
    }

    /**
     * Getter for the inputs id.
     * 
     * @return the inputs
     */
    public CheckResult[] getInputs() {
        return this.inputs.clone();
    }

    /**
     * Getter for the container
     * 
     * @return the container
     */
    public MetricDescriptor[] getContainer() {
        return this.container.clone();
    }

    /**
     * Setter for the inputs.
     * 
     * @param pInputs
     *            the inputs to set
     */
    public void setInputs(final CheckResult[] pInputs) {
        this.inputs = pInputs.clone();
    }

    /**
     * Setter for the container
     * 
     * @param pContainer
     *            the container to set
     */
    public void setContainer(final MetricDescriptor[] pContainer) {
        this.container = pContainer.clone();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public IStatus run(final IProgressMonitor monitor) {
        LOGGER.finest("Begin run method");

        // Instantiate return variable
        final IStatus status = Status.OK_STATUS;
        final int totalWork = this.inputs.length;

        // Instantiate descriptors
        final List<MetricDescriptor> descriptors = new LinkedList<MetricDescriptor>();
        // final FileMetricDescriptor file = new FileMetricDescriptor();
        // final FunctionMetricDescriptor function = new
        // FunctionMetricDescriptor();

        // Start converting
        monitor.beginTask("Converting...", totalWork);

        for (final CheckResult checker : this.inputs) {
            /*
             * 1. Defining which informations of the checker already have been
             * fulfilled in the descriptors.
             */
            int metricIndex = 0;
            int fileIndex = 0;
            boolean metricDefined = false;
            boolean fileDefined = false;
            while (metricIndex < descriptors.size() && !metricDefined) {
                final MetricDescriptor metric = descriptors.get(metricIndex);
                if (checker.getName().equals(metric.getName())) {
                    metricDefined = true;
                    while (fileIndex < metric.getDescriptors().size() && !fileDefined) {
                        final FileMetricDescriptor file = metric.getDescriptors().get(fileIndex);
                        if (file.getFilePath().toFile().getAbsolutePath()
                                        .equals(checker.getFile().getAbsolutePath())) {
                            fileDefined = true;
                        } else {
                            fileIndex++;
                        }
                    }
                } else {
                    metricIndex++;
                }
            }
            /*
             * 2. Writing information into the descriptors
             */
            /*
             * 2.1 Defining the metric if not defined
             */
            final MetricDescriptor metric;
            if (!metricDefined) {
                metric = new MetricDescriptor(checker.getName());
                descriptors.add(metric);
            } else {
                metric = descriptors.get(metricIndex);
            }
            final FileMetricDescriptor file;
            /*
             * 2.2 If the checker is defined for a file. The job is done once
             * FileMetricDescriptor is updated or created.
             */
            if (checker.getLocation() == null || checker.getLocation().isEmpty()) {
                if (!fileDefined) {
                    file = new FileMetricDescriptor(new Path(checker.getFile().getAbsolutePath()),
                                    checker.getValue());
                    metric.getDescriptors().add(file);
                } else {
                    file = metric.getDescriptors().get(fileIndex);
                    file.setValue(checker.getValue());
                }
            } else {
                /*
                 * 2.3 If the checker is not defined for a file, then,
                 * FileMetricDescriptor must be created without value and
                 * FunctionMetricDescriptor added to it.
                 */
                if (!fileDefined) {
                    file = new FileMetricDescriptor();
                    file.setFilePath(new Path(checker.getFile().getAbsolutePath()));
                    metric.getDescriptors().add(file);
                } else {
                    file = metric.getDescriptors().get(fileIndex);
                }
                file.getDescriptors().add(new FunctionMetricDescriptor(checker.getId(),
                                checker.getLocation(), checker.getValue(),
                                new Path(checker.getFile().getAbsolutePath()), checker.getLine()));
            }
        }
        this.container = descriptors.toArray(new MetricDescriptor[descriptors.size()]);

        LOGGER.finest("End run method");
        return status;
    }

}
