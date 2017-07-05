package fr.cnes.analysis.tools.ui.configurations;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

public class ConfigurationService {

    public static final String CONFIGURATION_EP_ID = "fr.cnes.analysis.tools.ui.configuration";
    public static final String CONFIGURATION_EP_NAME = "configuration";

    public static final String CONFIGURATION_EP_EL_CONFIGURATION = "configuration";
    public static final String CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME = "name";
    public static final String CONFIGURATION_EP_EL_CONFIGURATION_ATT_DESCRIPTION = "description";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION = "checkConfiguration";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_CHECKID = "checkId";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_NAME = "name";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_DESCRIPTION = "description";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_ENABLED = "enabled";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MAXVALUE = "maxValue";
    public static final String CONFIGURATION_EP_EL_CHECKCONFIGURATION_ATT_MINVALUE = "minValue";

    /**
     * @return All the configurations in contribution of
     *         {@link #CONFIGURATION_EP_ID}.
     */
    public static List<ConfigurationContainer> getConfigurations() {
        List<ConfigurationContainer> configurations = new ArrayList<>();
        IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        for (IConfigurationElement configuration : configurationsContributors) {
            List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            ConfigurationContainer configContainer = new ConfigurationContainer(
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

    public static ConfigurationContainer getConfigurations(String configurationName)
            throws NullContributionException {
        List<ConfigurationContainer> configurations = new ArrayList<>();
        IConfigurationElement[] configurationsContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CONFIGURATION_EP_ID);
        for (IConfigurationElement configuration : configurationsContributors) {
            List<CheckConfigurationContainer> checkersConfigurations = new ArrayList<>();
            if (configuration.getAttribute(CONFIGURATION_EP_EL_CONFIGURATION_ATT_NAME)
                    .equals(configurationName)) {
                ConfigurationContainer configContainer = new ConfigurationContainer(
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
