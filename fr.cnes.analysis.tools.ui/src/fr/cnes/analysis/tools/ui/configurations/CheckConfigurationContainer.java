/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.configurations;

/**
 * This class is a data structure being used by {@link ConfigurationService}.
 */
public class CheckConfigurationContainer {

    /** Checker identifier */
    private String checkId;
    /** Checker name */
    private String name;
    /** Checker configuration's description */
    private String description;
    /** Checker is enabled */
    private boolean enabled;
    /** Max value set for the Checker */
    private Float maxValue;
    /** Min value set for the Checker */
    private Float minValue;

    /**
     * @param pCheckId
     *            Checker identifier
     * @param pName
     *            Checker name
     * @param pDescription
     *            Checker configuration's description
     * @param pEnabled
     *            Checker is enabled
     * @param pMaxValue
     *            Max value set for the Checker
     * @param pMinValue
     *            Min value set for the Checker
     */
    public CheckConfigurationContainer(String pCheckId, String pName, String pDescription,
                    Boolean pEnabled, Float pMaxValue, Float pMinValue) {
        super();
        this.checkId = pCheckId;
        this.name = pName;
        this.description = pDescription;
        this.enabled = pEnabled.booleanValue();
        this.maxValue = pMaxValue;
        this.minValue = pMinValue;
    }

    /**
     * @return the checkId
     */
    public final String getCheckId() {
        return checkId;
    }

    /**
     * @param pCheckId
     *            the checkId to set
     */
    public final void setCheckId(String pCheckId) {
        this.checkId = pCheckId;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param pName
     *            the name to set
     */
    public final void setName(String pName) {
        this.name = pName;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param pDescription
     *            the description to set
     */
    public final void setDescription(String pDescription) {
        this.description = pDescription;
    }

    /**
     * @return the enabled
     */
    public final boolean isEnabled() {
        return enabled;
    }

    /**
     * @param pEnabled
     *            the enabled to set
     */
    public final void setEnabled(boolean pEnabled) {
        this.enabled = pEnabled;
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        return maxValue;
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(Float pMaxValue) {
        this.maxValue = pMaxValue;
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        return minValue;
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(Float pMinValue) {
        this.minValue = pMinValue;
    }

}
