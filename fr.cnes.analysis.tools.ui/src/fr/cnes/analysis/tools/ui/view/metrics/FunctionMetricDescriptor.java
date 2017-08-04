/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Descriptor for metric's value of a function.
 * 
 * 
 */
public class FunctionMetricDescriptor implements IMetricDescriptor, Cloneable {

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
        this.metricId = "";
        this.location = "";
        this.value = Float.valueOf(0.0f);
        this.line = Integer.valueOf(0);
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
        this.metricId = pId;
        this.filePath = pFilePath;
        this.location = pLocation;
        this.value = pValue;
        this.line = pLine;
    }

    /**
     * Getter for line.
     * 
     * @return line of the metric
     */
    public Integer getLine() {
        return line;
    }

    /**
     * Setter for line.
     * 
     * @param pLine
     *            new line
     */
    public void setLine(final Integer pLine) {
        this.line = pLine;
    }

    /**
     * @return the path of the file containing the Metric
     */
    public IPath getFilePath() {
        return filePath;
    }

    /**
     * Set the filePath of the file containing the Metric
     * 
     * @param pFilePath
     *            the new Path to set
     */
    public void setFilePath(final IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * Getter for the id.
     * 
     * @return the id
     */
    public String getMetricId() {
        return this.metricId;
    }

    /**
     * Getter for the location.
     * 
     * @return the location
     */
    public String getLocation() {
        return this.location;
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

    /**
     * Setter for the id.
     * 
     * @param pId
     *            the id to set
     */
    public void setMetricId(final String pId) {
        this.metricId = pId;
    }

    /**
     * Setter for the location.
     * 
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        this.location = pLocation;
    }

    /**
     * Setter for the value.
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
        return this.location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getMean()
     */
    @Override
    public Float getMean() {
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
        boolean result = true;
        if (UserPreferencesService.hasMaxValue(this.getMetricId())) {
            result = this.getValue()
                            .compareTo(UserPreferencesService.getMaxValue(this.getMetricId())) > 0;
        }
        if (UserPreferencesService.hasMinValue(this.getMetricId())) {
            result = this.getValue()
                            .compareTo(UserPreferencesService.getMinValue(this.getMetricId())) < 0;
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
        if (object instanceof FunctionMetricDescriptor) {
            isEqual = this.location.equals(((FunctionMetricDescriptor) object).getLocation())
                            && this.metricId.equals(
                                            ((FunctionMetricDescriptor) object).getMetricId());
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
    public FunctionMetricDescriptor clone() throws CloneNotSupportedException {
        final FunctionMetricDescriptor clone = (FunctionMetricDescriptor) super.clone();
        clone.setMetricId(this.metricId);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        return clone;
    }
}
