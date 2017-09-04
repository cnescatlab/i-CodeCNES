package fr.cnes.analysis.tools.ui.configurations;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

import java.util.ArrayList;
import java.util.List;

import fr.cnes.analysis.tools.ui.logger.UILogger;

/**
 * This class is a data structure being used by {@link ConfigurationService}.
 *
 */
public class ConfigurationContainer {

    /** Class name **/
    private static final String CLASS = ConfigurationContainer.class.getName();
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
        final String method = "ConfigurationContainer";
        UILogger.entering(CLASS, method, new Object[] {
            pName, pDescription
        });
        this.name = pName;
        this.description = pDescription;
        this.checkConfigurations = new ArrayList<>();
        UILogger.exiting(CLASS, method);
    }

    /**
     * @return the name
     */
    public final String getName() {
        final String method = "getName";
        UILogger.entering(CLASS, method);
        UILogger.exiting(CLASS, method, name);
        return name;
    }

    /**
     * @param pName
     *            the name to set
     */
    public final void setName(String pName) {
        final String method = "setName";
        UILogger.entering(CLASS, method, pName);
        this.name = pName;
        UILogger.exiting(CLASS, method);
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        final String method = "getDescription";
        UILogger.entering(CLASS, method);
        UILogger.exiting(CLASS, method, description);
        return description;
    }

    /**
     * @param pDescription
     *            the description to set
     */
    public final void setDescription(String pDescription) {
        final String method = "setDescription";
        UILogger.entering(CLASS, method, pDescription);
        this.description = pDescription;
        UILogger.exiting(CLASS, method);
    }

    /**
     * @return the checkConfigurations
     */
    public final List<CheckConfigurationContainer> getCheckConfigurations() {
        final String method = "getCheckConfigurations";
        UILogger.entering(CLASS, method);
        UILogger.exiting(CLASS, method, checkConfigurations);
        return checkConfigurations;
    }

    /**
     * @param pCheckConfigurations
     *            the checkConfigurations to set
     */
    public final void setCheckConfigurations(
                    List<CheckConfigurationContainer> pCheckConfigurations) {
        final String method = "setCheckConfigurations";
        UILogger.entering(CLASS, method, pCheckConfigurations);
        this.checkConfigurations = pCheckConfigurations;
        UILogger.exiting(CLASS, method);
    }

}
