/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;

/**
 * Job used to converter inputs from analysis to valuable inputs for the
 * MetricView.
 * 
 */
public class MetricConverter extends Job {
    /** Logger. **/
    public final static Logger LOGGER = Logger.getLogger(MetricConverter.class.getName());

    /** The original inputs. **/
    private FileValue[]        inputs;
    /** A value container which has all values of rules. **/
    private MetricDescriptor[] container;

    /**
     * Empty constructor for this Job.
     */
    public MetricConverter() {
        super("Converting results...");
        this.inputs = new FileValue[0];
        this.container = new MetricDescriptor[0];

    }

    /**
     * Constructor for this Job with an array of violations.
     * 
     * @param pInputs
     *            the inputs
     */
    public MetricConverter(final FileValue[] pInputs) {
        super("Converting results...");
        this.inputs = pInputs.clone();
        this.container = new MetricDescriptor[0];
    }

    /**
     * Getter for the inputs id.
     * 
     * @return the inputs
     */
    public FileValue[] getInputs() {
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
    public void setInputs(final FileValue[] pInputs) {
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
        IStatus status = Status.OK_STATUS;
        final int totalWork = this.inputs.length;

        // Instantiate descriptors
        final List<MetricDescriptor> descriptors = new LinkedList<MetricDescriptor>();
        final MetricDescriptor metric = new MetricDescriptor();
        final FileMetricDescriptor file = new FileMetricDescriptor();
        final FunctionMetricDescriptor function = new FunctionMetricDescriptor();

        // Start converting
        monitor.beginTask("Converting...", totalWork);
        try {
            for (final FileValue value : this.inputs) {
                if (descriptors.isEmpty() || !descriptors.get(descriptors.size() - 1).getName()
                        .equals(value.getMetricName())) {
                    metric.getDescriptors().clear();
                    metric.setName(value.getMetricName());
                    descriptors.add(metric.clone());
                }
                file.setFilePath(value.getFilePath());
                file.setValue(value.getValue());
                file.getDescriptors().clear();

                for (final FunctionValue fValue : value.getFunctionValues()) {
                    function.setMetricId(value.getMetricId());
                    function.setFilePath(value.getFilePath());
                    function.setLocation(fValue.getLocation());
                    function.setLine(fValue.getLine());
                    function.setValue(fValue.getValue());
                    if (!file.getDescriptors().contains(function)) {
                        file.getDescriptors().add(function.clone());

                    }
                }
                descriptors.get(descriptors.size() - 1).getDescriptors().add(file.clone());
                monitor.worked(1);
            }
            this.container = descriptors.toArray(new MetricDescriptor[descriptors.size()]);
        } catch (final CloneNotSupportedException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status = new Status(Status.ERROR, "fr.cnes.analysis.tools.fortran.analyzer",
                    Status.ERROR, exception.getMessage(), exception);
        }

        LOGGER.finest("End run method");
        return status;
    }

}
