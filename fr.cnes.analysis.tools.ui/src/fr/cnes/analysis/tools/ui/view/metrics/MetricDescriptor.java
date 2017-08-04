/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.LinkedList;
import java.util.List;

/**
 * Class for general description of a metric.
 * 
 */
public class MetricDescriptor implements IMetricDescriptor, Cloneable {

    /** Name of the metric. **/
    private String name;
    /** List of descriptor for the files. **/
    private List<FileMetricDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public MetricDescriptor() {
        this.name = "";
        this.descriptors = new LinkedList<FileMetricDescriptor>();
    }

    /**
     * Constructor with metric's name.
     * 
     * @param pName
     *            metric's name
     */
    public MetricDescriptor(final String pName) {
        this.name = pName;
        this.descriptors = new LinkedList<FileMetricDescriptor>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * Getter for descriptors.
     * 
     * @return the descriptors.
     */
    public List<FileMetricDescriptor> getDescriptors() {
        return this.descriptors;
    }

    /**
     * Setter for the name.
     * 
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        this.name = pName;
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FileMetricDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getValue()
     */
    @Override
    public Float getValue() {
        Float value = Float.valueOf(0.0f);
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            value = Float.valueOf(value.floatValue() + descriptor.getValue().floatValue());
        }
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMean()
     */
    @Override
    public Float getMean() {
        float mean = 0.0f;
        float totalSize = 0.0f;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            mean = mean + descriptor.getMean().floatValue() * descriptor.getDescriptors().size();
            totalSize = totalSize + descriptor.getDescriptors().size();
        }
        return Float.valueOf(mean / totalSize);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMinimum()
     */
    @Override
    public Float getMinimum() {
        float min = Float.NaN;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMinimum().floatValue() < min || Float.isNaN(min)) {
                min = descriptor.getMinimum().floatValue();
            }
        }
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
        float max = Float.NaN;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMaximum().floatValue() > max || Float.isNaN(max)) {
                max = descriptor.getMaximum().floatValue();
            }
        }
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
        final float min = Float.NaN;
        String minCause = "";
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMinimum().floatValue() < min || Float.isNaN(min)) {
                minCause = descriptor.getName();
            }
        }
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
        final float max = Float.NaN;
        String maxCause = "";
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getMaximum().floatValue() > max || Float.isNaN(max)) {
                maxCause = descriptor.getName();
            }
        }
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
        boolean result = true;
        for (final FileMetricDescriptor descriptor : this.descriptors) {
            result = result && descriptor.hasRightValue();
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        final boolean isEqual;
        if (object instanceof MetricDescriptor) {
            isEqual = this.name.equals(((MetricDescriptor) object).getName());
        } else {
            isEqual = false;
        }
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return this.descriptors.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public MetricDescriptor clone() throws CloneNotSupportedException {
        final MetricDescriptor clone = (MetricDescriptor) super.clone();
        clone.setName(this.name);
        clone.setDescriptors(new LinkedList<FileMetricDescriptor>(this.descriptors));
        return clone;
    }

}
