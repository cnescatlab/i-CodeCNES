package fr.cnes.analysis.tools.ui.configurations;

import java.util.ArrayList;
import java.util.List;

public class ConfigurationContainer {
    private String name;
    private String description;
    private List<CheckConfigurationContainer> checkConfigurations;

    public ConfigurationContainer(String pName, String pDescription) {
        this.name = pName;
        this.description = pDescription;
        this.checkConfigurations = new ArrayList<>();
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
     * @return the checkConfigurations
     */
    public final List<CheckConfigurationContainer> getCheckConfigurations() {
        return checkConfigurations;
    }

    /**
     * @param checkConfigurations
     *            the checkConfigurations to set
     */
    public final void setCheckConfigurations(
            List<CheckConfigurationContainer> checkConfigurations) {
        this.checkConfigurations = checkConfigurations;
    }

}
