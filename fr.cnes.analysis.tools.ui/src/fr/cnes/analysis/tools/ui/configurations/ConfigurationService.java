/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.configurations;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.ui.logger.UILogger;

/**
 * This class should be used to reach configurations data contributing to
 * {@link #CONFIGURATION_EP_ID}.
 * 
 * @since 3.0
 */
public final class ConfigurationService {

    /** Configuration EP identifier */
    public static final String CONFIGURATION_EP_ID = "fr.cnes.analysis.tools.ui.configuration";
    /** Configuration EP name */
    public static final String CONFIGURATION_EP_NAME = "configuration";
    /** Configuration Element */
    public static final String CONFIGURATION_EP_EL_CONFIGURATION = "configuration";
    /** Configuration Element's attribute name */
    public static final String CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME = "name";
    /** Configuration Element's attribute description */
    public static final String CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION = "description";
    /** CheckConfiguration Element */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION = "checkConfiguration";
    /** CheckConfiguration Element's attribute checkId */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID = "checkId";
    /** CheckConfiguration Element's attribute name */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME = "name";
    /** CheckConfiguration Element's attribute description */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION = "description";
    /** CheckConfiguration Element's attribute enabled */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED = "enabled";
    /** CheckConfiguration Element's attribute maxValue */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE = "maxValue";
    /** CheckConfiguration Element's attribute minValue */
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE = "minValue";

    /** Class name **/
    private static final String CLASS = ConfigurationService.class.getName();

    /**
     * This utility class should not be instantiated.
     */
    private ConfigurationService() {
        // Do not instantiate
    }

    /**
     * @return All the configurations in contribution of
     *         {@link #CONFIGURATION_EP_ID}.
     */
    public static List<ConfigurationContainer> getConfigurations() {
        final String method = "getConfigurations";
        UILogger.entering(CLASS, method);
        final List<ConfigurationContainer> configurations = new ArrayList<>();
        final IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        for (IConfigurationElement configuration : configurationsContributors) {
            final List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            final ConfigurationContainer configContainer = new ConfigurationContainer(
                            configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME),
                            configuration.getAttribute(
                                            CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION));
            for (IConfigurationElement checkerConfiguration : configuration
                            .getChildren(CONFIGURATION_EP_EL_CHECKCONFIGURATION)) {
                Boolean isEnabled = Boolean.TRUE;
                if (checkerConfiguration
                                .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED)
                                .equals("false")) {
                    isEnabled = Boolean.FALSE;
                }
                Float minValue, maxValue;
                try {
                    minValue = Float.valueOf(Float.parseFloat(checkerConfiguration.getAttribute(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE)));
                } catch (@SuppressWarnings("unused") NullPointerException exception) {
                    minValue = Float.valueOf(Float.NaN);
                }
                try {
                    maxValue = Float.valueOf(Float.parseFloat(checkerConfiguration.getAttribute(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE)));
                } catch (@SuppressWarnings("unused") NullPointerException exception) {
                    maxValue = Float.valueOf(Float.NaN);
                }
                checkersConfigurations.add(new CheckConfigurationContainer(
                                checkerConfiguration.getAttribute(
                                                CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID),
                                checkerConfiguration.getAttribute(
                                                CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME),
                                checkerConfiguration.getAttribute(
                                                CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION),
                                isEnabled, maxValue, minValue));
            }
            configContainer.setCheckConfigurations(checkersConfigurations);
            configurations.add(configContainer);
        }
        UILogger.exiting(CLASS, method, configurations);
        return configurations;
    }

    /**
     * @param configurationName
     *            The configuration to retrieve data from
     * @return A {@link ConfigurationContainer} containing all data contained
     *         for <code>configurationName</code>.
     * @throws NullContributionException
     *             when configuration requested do not exist or is trying to
     *             reach contribution not available/
     */
    public static ConfigurationContainer getConfigurations(String configurationName)
                    throws NullContributionException {
        final String method = "getConfigurations";
        UILogger.entering(CLASS, method, configurationName);
        final IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        boolean found = false;
        IConfigurationElement configuration;
        int configurationsCounter = 0;
        IConfigurationElement checkerConfiguration;
        int checkerConfigurationsCounter = 0;
        ConfigurationContainer configContainer = null;
        while (configurationsContributors.length > configurationsCounter && !found) {
            configuration = configurationsContributors[configurationsCounter];
            final List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            if (configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME)
                            .equals(configurationName)) {
                configContainer = new ConfigurationContainer(
                                configuration.getAttribute(
                                                CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME),
                                configuration.getAttribute(
                                                CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION));
                while (configuration.getChildren(
                                CONFIGURATION_EP_EL_CHECKCONFIGURATION).length > checkerConfigurationsCounter
                                && !found) {
                    checkerConfiguration = configuration.getChildren(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION)[checkerConfigurationsCounter];
                    Boolean isEnabled = Boolean.TRUE;
                    if (checkerConfiguration.getAttribute(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED)
                                    .equals("false")) {
                        isEnabled = Boolean.FALSE;
                    }
                    Float minValue, maxValue;
                    try {
                        minValue = Float.valueOf(Float.parseFloat(checkerConfiguration.getAttribute(
                                        CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE)));
                    } catch (@SuppressWarnings("unused") NullPointerException e) {
                        minValue = Float.valueOf(Float.NaN);
                    }
                    try {
                        maxValue = Float.valueOf(Float.parseFloat(checkerConfiguration.getAttribute(
                                        CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE)));
                    } catch (@SuppressWarnings("unused") NullPointerException e) {
                        maxValue = Float.valueOf(Float.NaN);
                    }
                    checkersConfigurations.add(new CheckConfigurationContainer(checkerConfiguration
                                    .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID),
                                    checkerConfiguration.getAttribute(
                                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME),
                                    checkerConfiguration.getAttribute(
                                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION),
                                    isEnabled, maxValue, minValue));
                    checkerConfigurationsCounter++;
                }
                configContainer.setCheckConfigurations(checkersConfigurations);
                found = true;
            }
            configurationsCounter++;
        }

        if (found) {
            UILogger.exiting(CLASS, method, configContainer);
            return configContainer;
        }
        final NullContributionException exception = new NullContributionException(
                        "Impossible to find " + configurationName + " in " + CONFIGURATION_EP_ID
                                        + " contributors.");
        UILogger.throwing(CLASS, method, exception);
        throw exception;

    }
}
