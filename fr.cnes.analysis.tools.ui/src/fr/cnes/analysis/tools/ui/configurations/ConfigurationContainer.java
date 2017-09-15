package fr.cnes.analysis.tools.ui.configurations;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

import java.util.ArrayList;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

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
        ICodeLogger.entering(CLASS, method, new Object[] {
            pName, pDescription
        });
        this.name = pName;
        this.description = pDescription;
        this.checkConfigurations = new ArrayList<>();
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
     * @return the checkConfigurations
     */
    public final List<CheckConfigurationContainer> getCheckConfigurations() {
        final String method = "getCheckConfigurations";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checkConfigurations);
        return checkConfigurations;
    }

    /**
     * @param pCheckConfigurations
     *            the checkConfigurations to set
     */
    public final void setCheckConfigurations(
                    List<CheckConfigurationContainer> pCheckConfigurations) {
        final String method = "setCheckConfigurations";
        ICodeLogger.entering(CLASS, method, pCheckConfigurations);
        this.checkConfigurations = pCheckConfigurations;
        ICodeLogger.exiting(CLASS, method);
    }

}
