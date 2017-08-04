/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

/**
 * Class descriptor for metric value in a file.
 * 
 * 
 */
public class FileMetricDescriptor implements IMetricDescriptor, Cloneable {

    /** File's path. **/
    private IPath filePath;
    /** Value for the file. **/
    private Float value;
    /** List of descriptors for the function in the file. **/
    private List<FunctionMetricDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public FileMetricDescriptor() {
        this.value = Float.valueOf(0.0f);
        this.descriptors = new LinkedList<FunctionMetricDescriptor>();
    }

    /**
     * Constructor with file's path and the value.
     * 
     * @param pFilePath
     *            the file's path
     * @param pValue
     *            the value
     */
    public FileMetricDescriptor(final IPath pFilePath, final Float pValue) {
        this.filePath = pFilePath;
        this.value = pValue;
        this.descriptors = new LinkedList<FunctionMetricDescriptor>();
    }

    /**
     * Getter for the file's path.
     * 
     * @return the file's path
     */
    public IPath getFilePath() {
        return this.filePath;
    }

    /**
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FunctionMetricDescriptor> getDescriptors() {
        return this.descriptors;
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FunctionMetricDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /**
     * Setter for the value
     * 
     * @param pValue
     *            the value to set
     */
    public void setValue(final Float pValue) {
        this.value = pValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.filePath.toFile().getName();
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getValue()
     */
    @Override
    public Float getValue() {
        return this.value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMean()
     */
    @Override
    public Float getMean() {
        Float mean = Float.valueOf(0.0f);
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            mean = Float.valueOf((mean.floatValue() + descriptor.getValue().floatValue()));
        }
        return Float.valueOf(mean.floatValue() / this.descriptors.size());
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() < min || Float.isNaN(min)) {
                min = descriptor.getValue().floatValue();
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() > max || Float.isNaN(max)) {
                max = descriptor.getValue().floatValue();
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() < min || Float.isNaN(min)) {
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() > max || Float.isNaN(max)) {
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
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
        if (object instanceof FileMetricDescriptor) {
            isEqual = this.filePath.equals(((FileMetricDescriptor) object).getFilePath());
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
        return this.value.intValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FileMetricDescriptor clone() throws CloneNotSupportedException {
        final FileMetricDescriptor clone = (FileMetricDescriptor) super.clone();
        clone.setValue(this.value);
        clone.setFilePath(this.filePath);
        clone.setDescriptors(new LinkedList<FunctionMetricDescriptor>(this.descriptors));
        return clone;
    }
}
