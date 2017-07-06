package fr.cnes.analysis.tools.ui.configurations;

public class CheckConfigurationContainer {

    private String checkId;
    private String name;
    private String description;
    private boolean enabled;
    private Float maxValue;
    private Float minValue;

    public CheckConfigurationContainer(String checkId, String name, String description,
            Boolean enabled, Float maxValue, Float minValue) {
        super();
        this.checkId = checkId;
        this.name = name;
        this.description = description;
        this.enabled = enabled;
        this.maxValue = maxValue;
        this.minValue = minValue;
    }

    /**
     * @return the checkId
     */
    public final String getCheckId() {
        return checkId;
    }

    /**
     * @param checkId
     *            the checkId to set
     */
    public final void setCheckId(String checkId) {
        this.checkId = checkId;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public final void setName(String name) {
        this.name = name;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public final void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the enabled
     */
    public final boolean isEnabled() {
        return enabled;
    }

    /**
     * @param enabled
     *            the enabled to set
     */
    public final void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public final void setMaxValue(Float maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        return minValue;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public final void setMinValue(Float minValue) {
        this.minValue = minValue;
    }

}
