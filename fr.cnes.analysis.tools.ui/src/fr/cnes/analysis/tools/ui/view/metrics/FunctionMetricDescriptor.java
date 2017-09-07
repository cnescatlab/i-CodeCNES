/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Descriptor for metric's value of a function.
 * 
 * 
 */
public class FunctionMetricDescriptor implements IMetricDescriptor, Cloneable {
    /** Class name */
    private static final String CLASS = FunctionMetricDescriptor.class.getName();

    /** Metric's id, used to find associated preferences. **/
    private String metricId;

    /** Metric's value's location. **/
    private String location;

    /** Metric's value **/
    private Float value;

    /** Path of the file containing the Metric **/
    private IPath filePath;

    /** Line of the metric. **/
    private Integer line;

    /**
     * Empty constructor.
     */
    public FunctionMetricDescriptor() {
        final String method = "FunctionMetricDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.metricId = "";
        this.location = "";
        this.value = Float.valueOf(0.0f);
        this.line = Integer.valueOf(0);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Constructor with all attributes initialized.
     * 
     * @param pId
     *            the id
     * @param pLocation
     *            the location
     * @param pValue
     *            the value
     * @param pFilePath
     *            the file containing the metric Path
     * @param pLine
     *            metric's line
     */
    public FunctionMetricDescriptor(final String pId, final String pLocation, final Float pValue,
                    final IPath pFilePath, final Integer pLine) {
        final String method = "FunctionMetricDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, pLocation, pValue, pFilePath, pLine
        });
        this.metricId = pId;
        this.filePath = pFilePath;
        this.location = pLocation;
        this.value = pValue;
        this.line = pLine;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for line.
     * 
     * @return line of the metric
     */
    public Integer getLine() {
        final String method = "getLine";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, line);
        return line;
    }

    /**
     * Setter for line.
     * 
     * @param pLine
     *            new line
     */
    public void setLine(final Integer pLine) {
        final String method = "setLine";
        ICodeLogger.entering(CLASS, method, pLine);
        this.line = pLine;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the path of the file containing the Metric
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, filePath);
        return filePath;
    }

    /**
     * Set the filePath of the file containing the Metric
     * 
     * @param pFilePath
     *            the new Path to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the id.
     * 
     * @return the id
     */
    public String getMetricId() {
        final String method = "getMetricId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.metricId);
        return this.metricId;
    }

    /**
     * Getter for the location.
     * 
     * @return the location
     */
    public String getLocation() {
        final String method = "getLocation";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.location);
        return this.location;
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

    /**
     * Setter for the id.
     * 
     * @param pId
     *            the id to set
     */
    public void setMetricId(final String pId) {
        final String method = "setMetricId";
        ICodeLogger.entering(CLASS, method, pId);
        this.metricId = pId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the location.
     * 
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        final String method = "setLocation";
        ICodeLogger.entering(CLASS, method, pLocation);
        this.location = pLocation;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the value.
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
        ICodeLogger.exiting(CLASS, method, this.location);
        return this.location;
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
        ICodeLogger.exiting(CLASS, method, Float.valueOf(Float.NaN));
        return Float.valueOf(Float.NaN);
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
        ICodeLogger.exiting(CLASS, method, Float.valueOf(Float.NaN));
        return Float.valueOf(Float.NaN);
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
        ICodeLogger.exiting(CLASS, method, Float.valueOf(Float.NaN));
        return Float.valueOf(Float.NaN);
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
        ICodeLogger.exiting(CLASS, method, "");
        return "";
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
        ICodeLogger.exiting(CLASS, method, "");
        return "";
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
        if (UserPreferencesService.hasMaxValue(this.getMetricId())) {
            result = this.getValue()
                            .compareTo(UserPreferencesService.getMaxValue(this.getMetricId())) > 0;
        }
        if (UserPreferencesService.hasMinValue(this.getMetricId())) {
            result = this.getValue()
                            .compareTo(UserPreferencesService.getMinValue(this.getMetricId())) < 0;
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
        if (object instanceof FunctionMetricDescriptor) {
            isEqual = this.location.equals(((FunctionMetricDescriptor) object).getLocation())
                            && this.metricId.equals(
                                            ((FunctionMetricDescriptor) object).getMetricId());
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
    public FunctionMetricDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final FunctionMetricDescriptor clone = (FunctionMetricDescriptor) super.clone();
        clone.setMetricId(this.metricId);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }
}
