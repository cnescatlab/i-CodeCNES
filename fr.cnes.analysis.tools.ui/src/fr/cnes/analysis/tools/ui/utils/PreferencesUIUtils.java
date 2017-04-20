/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.utils;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.preferencepage.metrics.MetricValues;

/**
 * Contains tools methods for preferences purpose.
 * 
 */
public final class PreferencesUIUtils {
    /** Logger **/
    public static final Logger LOGGER = Logger.getLogger(PreferencesUIUtils.class.getName());

    /** Extension point id **/
    public static final String PREF_EXT_PT_ID = "fr.cnes.analysis.tools.preferencePages.utils";
    /** Extension point id **/
    public static final String PREF_PAGE_EXT_PT_ID = "org.eclipse.ui.preferencePages";

    /** Preferences ids. */
    public static final String CRITICAL = ".Criticity";
    public static final String VALUE = ".Val.";
    public static final String LEVEL = ".Level";
    public static final String CUSTOM = ".Custom";
    public static final String SYMETRY = ".Sym.";

    /** Static value for extension point attribute called parentId. **/
    public static final String PARENT_ID = "parentId";

    /** Static value for extension point attribute called metricPageId. **/
    public static final String METRIC_PAGE_ID = "metricPageId";

    /** Static value for extension point attribute called rulePageId. **/
    public static final String RULE_PAGE_ID = "rulePageId";

    /** Static value for extension point attribute called name. **/
    public static final String CONTRIB_NAME = "name";

    /** Static value for extension point attribute called id. **/
    public static final String CONTRIB_ID = "id";

    /** Static value for extension point attribute called metricExtensionId. **/
    public static final String METRIC_EXT_ID = "metricExtensionId";

    /** Static value for extension point attribute called ruleExtensionId. **/
    public static final String RULE_EXT_ID = "ruleExtensionId";

    /** Critical Level. */
    public static String[] LEVELS = { "A", "B", "C", "D", "Custom" };

    /**
     * Protected constructor. This class should not be instantiated.
     */
    private PreferencesUIUtils() {
        // do nothing
    }

    /**
     * Initialize all preferences used in iCode.
     */
    public static void initializePreferences() {
        LOGGER.finest("Begin initializePreferences method");

        final IPreferenceStore store = PlatformUI.getPreferenceStore();

        final IConfigurationElement[] contributions = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(PREF_EXT_PT_ID);

        for (final IConfigurationElement contribution : contributions) {
            // Language preference
            store.setDefault(contribution.getAttribute(PARENT_ID), true);

            // Rule enabled and metric enabled preferences
            store.setDefault(contribution.getAttribute(RULE_PAGE_ID), true);
            store.setDefault(contribution.getAttribute(METRIC_PAGE_ID), true);

            // Rule preferences
            final IConfigurationElement[] ruleContrib = Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(contribution.getAttribute(RULE_EXT_ID));

            for (final IConfigurationElement ruleContribution : ruleContrib) {
                store.setDefault(ruleContribution.getAttribute(CONTRIB_ID), true);
                store.setDefault(ruleContribution.getAttribute(CONTRIB_ID) + CRITICAL, "Error");
            }

            // Metric preferences
            final IConfigurationElement[] metricContrib = Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(contribution.getAttribute(METRIC_EXT_ID));
            for (final IConfigurationElement metricContribution : metricContrib) {
                store.setDefault(metricContribution.getAttribute(CONTRIB_ID), true);

                final String metricId = metricContribution.getAttribute(CONTRIB_ID);
                final int index = getIndexOf(metricId);
                for (int i = 0; i < PreferencesUIUtils.LEVELS.length - 1; i++) {
                    store.setValue(
                            metricId + PreferencesUIUtils.VALUE + PreferencesUIUtils.LEVELS[i],
                            MetricValues.getVALUES()[i][index]);
                    store.setDefault(
                            metricId + PreferencesUIUtils.VALUE + PreferencesUIUtils.LEVELS[i],
                            MetricValues.getVALUES()[i][index]);
                }

                store.setDefault(metricId + PreferencesUIUtils.VALUE + PreferencesUIUtils.LEVELS[4],
                        MetricValues.getVALUES()[2][index]);

                store.setValue(metricId + PreferencesUIUtils.SYMETRY,
                        MetricValues.getSYMBOL()[index]);
            }

            store.setDefault(LEVEL, 2);
        }

        LOGGER.finest("End initializePreferences method");
    }

    /**
     * Get the index of the metric.
     * 
     * @param value
     *            the value
     * @return the index.
     */
    private static int getIndexOf(String value) {
        int index = -1;
        for (int i = 0; i < MetricValues.getVALUES().length; i++) {
            if (value.contains(MetricValues.getMETRICS()[i])) {
                index = i;
            }
        }
        return index;
    }

    /**
     * Get all contributions of preferences extension point.
     * 
     * @return preferences extension point's contributions.
     */
    public static IConfigurationElement[] getAllContributions() {
        return Platform.getExtensionRegistry().getConfigurationElementsFor(PREF_EXT_PT_ID);
    }

    /**
     * Get a specific contribution considering its id.
     * 
     * @param contributionId
     *            the id of the contribution to get
     * @return the considered contribution, null if none is found
     */
    public static IConfigurationElement getContribution(final String contributionId) {
        LOGGER.finest("Begin getContribution method");

        IConfigurationElement contribution = null;
        for (final IConfigurationElement contrib : getAllContributions()) {
            if (contrib.getAttribute(CONTRIB_ID).equals(contributionId)) {
                contribution = contrib;
            }
        }

        LOGGER.finest("End getContribution method");
        return contribution;
    }

    /**
     * Set data for preference page implementing IExecutableExtension.
     * 
     * @param config
     *            the configuration
     * @param propertyName
     *            the property name
     * @param data
     *            the data (which should be a String)
     * @return the configuration element corresponding
     * @throws CoreException
     *             when data is not a String or the configuration element is not
     *             found
     */
    public static IConfigurationElement setInitializationData(final IConfigurationElement config,
            final String propertyName, final Object data) throws CoreException {
        LOGGER.finest("Begin setInitializationData method");

        // Local variable which store the contribution
        IConfigurationElement extension = null;

        // The input data should be a String
        if (data instanceof String) {

            // Search for the corresponding contribution
            for (final IConfigurationElement contribution : Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(PreferencesUIUtils.PREF_EXT_PT_ID)) {

                if (contribution.getAttribute(CONTRIB_ID).equals(data)) {
                    extension = contribution;
                }
            }
            // If none is found, throw an error
            if (extension == null) {
                final CoreException exception = new CoreException(new Status(IStatus.ERROR,
                        PlatformUI.PLUGIN_ID, 0,
                        "Impossible to find corresponding contribution for Preference", null));
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                throw exception;
            }
        } else {
            // If wrong instance type, throw an error
            final CoreException exception = new CoreException(
                    new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, 0,
                            "Data argument must be a String for Preference", null));
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            throw exception;
        }

        LOGGER.finest("End setInitializationData method");
        return extension;
    }

}
