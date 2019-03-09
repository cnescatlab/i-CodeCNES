/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * Class descriptor for metric value in a file.
 * 
 * 
 */
public class FileMetricDescriptor implements IMetricDescriptor, Cloneable {

    /** Class name */
    private static final String CLASS = FileMetricDescriptor.class.getName();

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
        final String method = "FileMetricDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.value = Float.valueOf(0.0f);
        this.descriptors = new LinkedList<FunctionMetricDescriptor>();
        ICodeLogger.exiting(CLASS, method);
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
        final String method = "FileMetricDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pFilePath, pValue
        });
        this.filePath = pFilePath;
        this.value = pValue;
        this.descriptors = new LinkedList<FunctionMetricDescriptor>();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the file's path.
     * 
     * @return the file's path
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.filePath);
        return this.filePath;
    }

    /**
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FunctionMetricDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.descriptors);
        return this.descriptors;
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FunctionMetricDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = new ArrayList<>(pDescriptors);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the value
     * 
     * @param pValue
     *            the value to set
     */
    public void setValue(final Float pValue) {
        final String method = "setValue";
        ICodeLogger.entering(CLASS, method, pValue);
        this.value = pValue;
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
        final String name = this.filePath.toFile().getName();
        ICodeLogger.exiting(CLASS, method, name);
        return name;
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
        ICodeLogger.exiting(CLASS, method, this.value);
        return this.value;
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
        Float mean = Float.valueOf(0.0f);
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            mean = Float.valueOf((mean.floatValue() + descriptor.getValue().floatValue()));
        }
        mean = Float.valueOf(mean.floatValue() / this.descriptors.size());
        ICodeLogger.exiting(CLASS, method, mean);
        return mean;
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() < min || Float.isNaN(min)) {
                min = descriptor.getValue().floatValue();
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() > max || Float.isNaN(max)) {
                max = descriptor.getValue().floatValue();
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() < min || Float.isNaN(min)) {
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
        for (final FunctionMetricDescriptor descriptor : this.descriptors) {
            if (descriptor.getValue().floatValue() > max || Float.isNaN(max)) {
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
        int counter = 0;
        while (result && this.descriptors.size() > counter) {
            result = this.descriptors.get(counter).hasRightValue();
            counter++;
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
        if (object instanceof FileMetricDescriptor) {
            isEqual = this.filePath.equals(((FileMetricDescriptor) object).getFilePath());
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
        ICodeLogger.exiting(CLASS, method, this.value);
        return this.value.intValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FileMetricDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final FileMetricDescriptor clone = (FileMetricDescriptor) super.clone();
        clone.setValue(this.value);
        clone.setFilePath(this.filePath);
        clone.setDescriptors(new LinkedList<FunctionMetricDescriptor>(this.descriptors));
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }
}
