/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.LinkedList;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * Class for general description of a metric.
 * 
 */
public class MetricDescriptor implements IMetricDescriptor, Cloneable {

    /** Class name **/
    private static final String CLASS = MetricDescriptor.class.getName();

    /** Name of the metric. **/
    private String name;
    /** List of descriptor for the files. **/
    private List<FileMetricDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public MetricDescriptor() {
        final String method = "MetricDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.name = "";
        this.descriptors = new LinkedList<FileMetricDescriptor>();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Constructor with metric's name.
     * 
     * @param pName
     *            metric's name
     */
    public MetricDescriptor(final String pName) {
        final String method = "MetricDescriptor";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        this.descriptors = new LinkedList<FileMetricDescriptor>();
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getName()
     */
    @Override
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, name);
        return this.name;
    }

    /**
     * Getter for descriptors.
     * 
     * @return the descriptors.
     */
    public List<FileMetricDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, descriptors);
        return this.descriptors;
    }

    /**
     * Setter for the name.
     * 
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FileMetricDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = pDescriptors;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getValue()
     */
    @Override
    public Float getValue() {
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        Float value = Float.valueOf(0.0f);
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            value = Float.valueOf(value.floatValue() + descriptor.getValue().floatValue());
        }
        ICodeLogger.exiting(CLASS, method, value);
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMean()
     */
    @Override
    public Float getMean() {
        final String method = "getMean";
        ICodeLogger.entering(CLASS, method);
        float mean = 0.0f;
        float totalSize = 0.0f;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            mean = mean + descriptor.getMean().floatValue() * descriptor.getDescriptors().size();
            totalSize = totalSize + descriptor.getDescriptors().size();
        }
        mean = mean / totalSize;
        ICodeLogger.exiting(CLASS, method, Float.valueOf(mean));
        return Float.valueOf(mean);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMinimum()
     */
    @Override
    public Float getMinimum() {
        final String method = "getMinimum";
        ICodeLogger.entering(CLASS, method);
        float min = Float.NaN;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMinimum().floatValue() < min || Float.isNaN(min)) {
                min = descriptor.getMinimum().floatValue();
            }
        }
        ICodeLogger.exiting(CLASS, method, Float.valueOf(min));
        return Float.valueOf(min);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMaximum()
     */
    @Override
    public Float getMaximum() {
        final String method = "getMaximum";
        ICodeLogger.entering(CLASS, method);
        float max = Float.NaN;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMaximum().floatValue() > max || Float.isNaN(max)) {
                max = descriptor.getMaximum().floatValue();
            }
        }
        ICodeLogger.exiting(CLASS, method, Float.valueOf(max));
        return Float.valueOf(max);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMinCause()
     */
    @Override
    public String getMinCause() {
        final String method = "getMinCause";
        ICodeLogger.entering(CLASS, method);
        final float min = Float.NaN;
        String minCause = "";
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMinimum().floatValue() < min || Float.isNaN(min)) {
                minCause = descriptor.getName();
            }
        }
        ICodeLogger.exiting(CLASS, method, minCause);
        return minCause;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMaxCause()
     */
    @Override
    public String getMaxCause() {
        final String method = "getMaxCause";
        ICodeLogger.entering(CLASS, method);
        final float max = Float.NaN;
        String maxCause = "";
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMaximum().floatValue() > max || Float.isNaN(max)) {
                maxCause = descriptor.getName();
            }
        }
        ICodeLogger.exiting(CLASS, method, maxCause);
        return maxCause;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#hasRightValue()
     */
    @Override
    public boolean hasRightValue() {
        final String method = "hasRightValue";
        ICodeLogger.entering(CLASS, method);
        boolean result = true;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            result = result && descriptor.hasRightValue();
        }
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(result));
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        final String method = "equals";
        ICodeLogger.entering(CLASS, method, object);
        final boolean isEqual;
        if (object instanceof MetricDescriptor) {
            isEqual = this.name.equals(((MetricDescriptor) object).getName());
        } else {
            isEqual = false;
        }
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(isEqual));
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final String method = "hashCode";
        ICodeLogger.entering(CLASS, method);
        assert false : "hashCode not designed";
        ICodeLogger.exiting(CLASS, method);
        return this.descriptors.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public MetricDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final MetricDescriptor clone = (MetricDescriptor) super.clone();
        clone.setName(this.name);
        clone.setDescriptors(new LinkedList<FileMetricDescriptor>(this.descriptors));
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }

}
