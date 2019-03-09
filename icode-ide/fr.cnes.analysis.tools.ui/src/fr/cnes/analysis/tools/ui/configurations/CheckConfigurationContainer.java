/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.configurations;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * This class is a data structure being used by {@link ConfigurationService}.
 */
public class CheckConfigurationContainer {

    /** Class name **/
    private static final String CLASS = CheckConfigurationContainer.class.getName();

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
        final String method = "CheckConfigurationContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pCheckId, pName, pDescription, pEnabled, pMaxValue, pMinValue
        });
        this.checkId = pCheckId;
        this.name = pName;
        this.description = pDescription;
        this.enabled = pEnabled.booleanValue();
        this.maxValue = pMaxValue;
        this.minValue = pMinValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the checkId
     */
    public final String getCheckId() {
        final String method = "getCheckId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checkId);
        return checkId;
    }

    /**
     * @param pCheckId
     *            the checkId to set
     */
    public final void setCheckId(String pCheckId) {
        final String method = "setCheckId";
        ICodeLogger.entering(CLASS, method, pCheckId);
        this.checkId = pCheckId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the name
     */
    public final String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, name);
        return name;
    }

    /**
     * @param pName
     *            the name to set
     */
    public final void setName(String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        final String method = "getDescription";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, description);
        return description;
    }

    /**
     * @param pDescription
     *            the description to set
     */
    public final void setDescription(String pDescription) {
        final String method = "setDescription";
        ICodeLogger.entering(CLASS, method, pDescription);
        this.description = pDescription;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the enabled
     */
    public final boolean isEnabled() {
        final String method = "isEnabled";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(enabled));
        return enabled;
    }

    /**
     * @param pEnabled
     *            the enabled to set
     */
    public final void setEnabled(boolean pEnabled) {
        final String method = "setEnabled";
        ICodeLogger.entering(CLASS, method, Boolean.valueOf(pEnabled));
        this.enabled = pEnabled;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        final String method = "getMaxValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, maxValue);
        return maxValue;
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(Float pMaxValue) {
        final String method = "setMaxValue";
        ICodeLogger.entering(CLASS, method, pMaxValue);
        this.maxValue = pMaxValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        final String method = "getMinValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, minValue);
        return minValue;
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(Float pMinValue) {
        final String method = "setMinValue";
        ICodeLogger.entering(CLASS, method, pMinValue);
        this.minValue = pMinValue;
        ICodeLogger.exiting(CLASS, method);
    }

}
