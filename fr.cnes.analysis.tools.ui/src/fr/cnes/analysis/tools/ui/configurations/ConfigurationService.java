package fr.cnes.analysis.tools.ui.configurations;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

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
        final List<ConfigurationContainer> configurations = new ArrayList<>();
        final IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        for (IConfigurationElement configuration : configurationsContributors) {
            final List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            final ConfigurationContainer configContainer = new ConfigurationContainer(
                    configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME),
                    configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION));
            for (IConfigurationElement checkerConfiguration : configuration
                    .getChildren(CONFIGURATION_EP_EL_CHECKCONFIGURATION)) {
                Boolean isEnabled = true;
                if (checkerConfiguration
                        .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED)
                        .equals("false")) {
                    isEnabled = false;
                }
                Float minValue, maxValue;
                try {
                    minValue = Float.parseFloat(checkerConfiguration
                            .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE));
                } catch (NullPointerException e) {
                    minValue = Float.NaN;
                }
                try {
                    maxValue = Float.parseFloat(checkerConfiguration
                            .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE));
                } catch (NullPointerException e) {
                    maxValue = Float.NaN;
                }
                checkersConfigurations.add(new CheckConfigurationContainer(
                        checkerConfiguration
                                .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID),
                        checkerConfiguration
                                .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME),
                        checkerConfiguration.getAttribute(
                                CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION),
                        isEnabled, maxValue, minValue));
            }
            configContainer.setCheckConfigurations(checkersConfigurations);
            configurations.add(configContainer);
        }
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
        final List<ConfigurationContainer> configurations = new ArrayList<>();
        final IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        for (IConfigurationElement configuration : configurationsContributors) {
            final List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            if (configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME)
                    .equals(configurationName)) {
                final ConfigurationContainer configContainer = new ConfigurationContainer(
                        configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME),
                        configuration
                                .getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION));
                for (IConfigurationElement checkerConfiguration : configuration
                        .getChildren(CONFIGURATION_EP_EL_CHECKCONFIGURATION)) {
                    Boolean isEnabled = true;
                    if (checkerConfiguration
                            .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED)
                            .equals("false")) {
                        isEnabled = false;
                    }
                    Float minValue, maxValue;
                    try {
                        minValue = Float.parseFloat(checkerConfiguration
                                .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE));
                    } catch (NullPointerException e) {
                        minValue = Float.NaN;
                    }
                    try {
                        maxValue = Float.parseFloat(checkerConfiguration
                                .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE));
                    } catch (NullPointerException e) {
                        maxValue = Float.NaN;
                    }
                    checkersConfigurations.add(new CheckConfigurationContainer(
                            checkerConfiguration.getAttribute(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID),
                            checkerConfiguration
                                    .getAttribute(CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME),
                            checkerConfiguration.getAttribute(
                                    CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION),
                            isEnabled, maxValue, minValue));
                }
                configContainer.setCheckConfigurations(checkersConfigurations);
                return configContainer;
            }
        }
        throw new NullContributionException("Impossible to find " + configurationName + " in "
                + CONFIGURATION_EP_ID + " contributors.");
    }
}
