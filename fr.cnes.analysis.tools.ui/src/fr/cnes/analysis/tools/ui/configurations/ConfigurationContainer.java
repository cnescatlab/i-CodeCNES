package fr.cnes.analysis.tools.ui.configurations;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

import java.util.ArrayList;
import java.util.List;

/**
 * This class is a data structure being used by {@link ConfigurationService}.
 *
 */
public class ConfigurationContainer {
    /** Configuration's name */
    private String name;
    /** Configuration's description */
    private String description;
    /** Checker's configuration */
    private List<CheckConfigurationContainer> checkConfigurations;

    /**
     * @param pName
     *            Configuration's name
     * @param pDescription
     *            Configuration's description
     */
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
     * @return the checkConfigurations
     */
    public final List<CheckConfigurationContainer> getCheckConfigurations() {
        return checkConfigurations;
    }

    /**
     * @param pCheckConfigurations
     *            the checkConfigurations to set
     */
    public final void setCheckConfigurations(
                    List<CheckConfigurationContainer> pCheckConfigurations) {
        this.checkConfigurations = pCheckConfigurations;
    }

}
