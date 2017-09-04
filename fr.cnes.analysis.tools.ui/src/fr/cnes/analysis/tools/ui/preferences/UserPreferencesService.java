/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerContainer;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerService;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import fr.cnes.analysis.tools.ui.Activator;
import fr.cnes.analysis.tools.ui.configurations.CheckConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationService;
import fr.cnes.analysis.tools.ui.logger.UILogger;

/**
 *
 */
public class UserPreferencesService extends AbstractPreferenceInitializer {

    /** Preference key to access severity of a checker */
    public static final String PREF_SEVERITY_KEY = ".Severity";
    /** Preference value of SEVERITY for Error */
    public static final String PREF_SEVERITY_ERROR_VALUE = "Error";
    /** Preference value of SEVERITY for Warning */
    public static final String PREF_SEVERITY_WARNING_VALUE = "Warning";
    /** Preference value of SEVERITY for Info */
    public static final String PREF_SEVERITY_INFO_VALUE = "Info";
    /** Preference key to access Min of a checker */
    public static final String PREF_MIN_VALUE_KEY = ".Min";
    /** Preference key to access Max of a checker */
    public static final String PREF_MAX_VALUE_KEY = ".Max";
    /** Preference key to access severity of a checker */
    public static final String PREF_CONFIGURATION_KEY = "Configuration";
    /** Preference key to access severity of a checker */
    public static final String PREF_CONFIGURATION_CUSTOMVALUE = "Custom";

    /**
     * This method retrieves all contributor of analyzer using
     * {@link LanguageService} and {@link CheckerService} and set each languages
     * and checker, then sets :
     * <ul>
     * <li>Checked : <i>true</i></li>
     * <li>Severity : <i>Error</i></li>
     * <li>maxValue, minValue: <i>Pending the
     * {@link CheckerPreferencesContainer}</i></li>
     * </ul>
     * 
     * @throws NullContributionException
     *             when a contribution couldn't be reached in one of the
     *             extension points being used to initiate preferences.
     * @throws CoreException
     *             when an eclipse related method couldn't be executed
     */
    public static void initPreferences() throws NullContributionException, CoreException {
        final String method = "initPreferences";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        for (String languageId : LanguageService.getLanguagesIds()) {
            Activator.getDefault().getPreferenceStore().setDefault(languageId, true);
            for (CheckerContainer checker : CheckerService.getCheckers(languageId)) {
                Activator.getDefault().getPreferenceStore().setDefault(checker.getId(), true);
                Activator.getDefault().getPreferenceStore().setDefault(
                                checker.getId() + PREF_SEVERITY_KEY, PREF_SEVERITY_ERROR_VALUE);
                if (checker.isMetric()) {
                    Activator.getDefault().getPreferenceStore()
                                    .setDefault(checker.getId() + PREF_MAX_VALUE_KEY, Float.NaN);
                    Activator.getDefault().getPreferenceStore()
                                    .setDefault(checker.getId() + PREF_MIN_VALUE_KEY, Float.NaN);
                }
            }
        }
        Activator.getDefault().getPreferenceStore().setDefault(PREF_CONFIGURATION_KEY,
                        PREF_CONFIGURATION_CUSTOMVALUE);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
     * initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        final String method = "initializeDefaultPreferences";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        try {
            initPreferences();
        } catch (NullContributionException | CoreException e) {
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            Activator.PLUGIN_ID, e.getMessage());
            UILogger.error(this.getClass().getName(), method, e);
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
    }

    /**
     * @param languageId
     *            Identifier of the language to enable.
     * @return Language enabling success.
     */
    public static boolean enableLanguage(String languageId) {
        final String method = "enableLanguage";
        UILogger.entering(UserPreferencesService.class.getName(), method, languageId);
        boolean success = false;
        if (languageExists(languageId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, true);
            success = true;
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return success;
    }

    /**
     * @param languageId
     *            Language identifier to verify.
     * @return Whether or not the language is enabled.
     */
    public static boolean isEnabledLanguage(String languageId) {
        final String method = "isEnabledLanguage";
        UILogger.entering(UserPreferencesService.class.getName(), method, languageId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().getBoolean(languageId);
    }

    /**
     * @param checkerId
     *            Identifier of the checker.
     * @return Whether or not the language is enabled.
     */
    public static boolean isEnabledChecker(String checkerId) {
        final String method = "isEnabledChecker";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().getBoolean(checkerId);
    }

    /**
     * @param languageId
     *            Language identifier of language to disable.
     * @return Language disabling success.
     */
    public static boolean disableLanguage(String languageId) {
        final String method = "disableLanguage";
        UILogger.entering(UserPreferencesService.class.getName(), method, languageId);
        boolean success = false;
        if (languageExists(languageId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, false);
            success = true;
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return success;
    }

    /**
     * Enable a checker pending it's language. In case the language is disabled
     * it will be enabled too.
     * 
     * @param languageId
     *            language identifier of the checker
     * @param checkerId
     *            identifier of the checker to enable
     * @return checker enabling success
     */
    public static boolean enableChecker(String languageId, String checkerId) {
        final String method = "enableChecker";
        UILogger.entering(UserPreferencesService.class.getName(), method, new Object[] {
            languageId, checkerId
        });
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, true);
            Activator.getDefault().getPreferenceStore().setValue(checkerId, true);
            success = true;
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return success;
    }

    /**
     * Disable a checker pending it's language.
     * 
     * @param languageId
     *            language identifier of the checker
     * @param checkerId
     *            identifier of the checker to enable
     * @return checker disabling success
     */
    public static boolean disableChecker(String languageId, String checkerId) {
        final String method = "disableChecker";
        UILogger.entering(UserPreferencesService.class.getName(), method, new Object[] {
            languageId, checkerId
        });
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(checkerId, false);
            success = true;
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return success;
    }

    /**
     * This method verify and return for each languages contributing the
     * analyzer that is enabled in the preferences.
     * 
     * @return Language identifier of languages enabled in the
     *         {@link PreferenceStore}.
     */
    public static List<String> getEnabledLanguagesIds() {
        final String method = "";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        final List<String> languages = new ArrayList<>();
        for (String language : LanguageService.getLanguagesIds()) {
            if (isEnabledLanguage(language)) {
                languages.add(language);
            }
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method, languages);
        return languages;
    }

    /**
     * @return identifiers of checkers disabled
     */
    public static List<String> getDisabledCheckersIds() {
        final String method = "";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        final List<String> checkers = new ArrayList<>();
        for (String languageId : getEnabledLanguagesIds()) {
            for (String checkerId : CheckerService.getCheckersIds(languageId)) {
                if (!isEnabledChecker(checkerId)) {
                    checkers.add(checkerId);
                }
            }
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method, checkers);
        return checkers;
    }

    /**
     * This method verify and return for each languages contributing the
     * analyzer and it's checkers.
     * 
     * @return Language identifier of languages enabled in the
     *         {@link PreferenceStore}.
     * @throws CoreException
     *             on execution failure
     * @throws NullContributionException
     *             when no contribution can be found
     */
    public static List<LanguagePreferencesContainer> getLanguagesPreferences()
                    throws NullContributionException, CoreException {
        final String method = "getLanguagesPreferences";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        final List<LanguagePreferencesContainer> languagesPrefs = new ArrayList<>();
        for (LanguageContainer language : LanguageService.getLanguages()) {
            if (languageExists(language.getId())) {
                final boolean languageEnabled = isEnabledLanguage(language.getId());
                final List<CheckerPreferencesContainer> checkersPrefs = new ArrayList<>();
                for (CheckerContainer checker : CheckerService.getCheckers(language.getId())) {
                    if (checkerExists(language.getId(), checker.getId())) {
                        checkersPrefs.add(new CheckerPreferencesContainer(language.getId(),
                                        language.getName(), checker.getId(), checker.getName(),
                                        isEnabledChecker(checker.getId()),
                                        getCheckerSeverity(checker.getId()), checker.isMetric()));
                    }
                }
                languagesPrefs.add(new LanguagePreferencesContainer(language.getId(),
                                language.getName(), languageEnabled, checkersPrefs));

            }
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method, languagesPrefs);
        return languagesPrefs;
    }

    /**
     * @return checkers with value set in preferences
     * @throws NullContributionException
     *             if a checker can't be reached because not existing
     * @throws CoreException
     *             when a checker can be reached for some runtime reasons.
     */
    public static List<CheckerPreferencesContainer> getCheckersPreferences()
                    throws NullContributionException, CoreException {
        final String method = "getCheckersPreferences";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        final List<CheckerPreferencesContainer> checkPrefs = new ArrayList<>();
        for (CheckerContainer checker : CheckerService.getCheckers()) {
            final String languageId = checker.getLanguage().getId();
            final String languageName = checker.getLanguage().getName();
            final String severity = getCheckerSeverity(checker.getId());
            final boolean isEnabled = isEnabledChecker(checker.getId());
            if (checker.isMetric()) {
                final Float minValue;
                if (hasMinValue(checker.getId())) {
                    minValue = getMinValue(checker.getId());
                } else {
                    minValue = Float.valueOf(Float.NaN);
                }
                final Float maxValue;
                if (hasMaxValue(checker.getId())) {
                    maxValue = getMaxValue(checker.getId());
                } else {
                    maxValue = Float.valueOf(Float.NaN);
                }
                checkPrefs.add(new CheckerPreferencesContainer(languageId, languageName,
                                checker.getId(), checker.getName(), isEnabled, severity, minValue,
                                maxValue, true));
            } else {
                checkPrefs.add(new CheckerPreferencesContainer(languageId, languageName,
                                checker.getId(), checker.getName(), isEnabled, severity, false));
            }
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method, checkPrefs);
        return checkPrefs;
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return severity of the checker
     */
    public static String getCheckerSeverity(String checkerId) {
        final String method = "getCheckerSeverity";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().getString(checkerId + PREF_SEVERITY_KEY);
    }

    /**
     * @param languageId
     *            language of the checker
     * @param checkerId
     *            identifier of the checker
     * @param severity
     *            severity chosen for the checker
     * @return set of the severity success
     */
    public static boolean setCheckerSeverity(String languageId, String checkerId, String severity) {
        final String method = "setCheckerSeverity";
        UILogger.entering(UserPreferencesService.class.getName(), method, new Object[] {
            languageId, checkerId, severity
        });
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(checkerId + PREF_SEVERITY_KEY,
                            severity);
            success = true;
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return success;
    }

    /**
     * @param languageId
     *            language identifier of the checker
     * @param checkerId
     *            identifier of the checker
     * @return whether or not the preferences store contains this checker.
     */
    public static boolean checkerExists(String languageId, String checkerId) {
        final String method = "checkerExists";
        UILogger.entering(UserPreferencesService.class.getName(), method, new Object[] {
            languageId, checkerId
        });
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().contains(checkerId);
    }

    /**
     * @param configurationName
     *            Configuration name to set in preferences.
     * @throws NullContributionException
     *             when a contribution could not be reached.
     */
    public static void setConfiguration(String configurationName) throws NullContributionException {
        final String method = "setConfiguration";
        UILogger.entering(UserPreferencesService.class.getName(), method, configurationName);
        Activator.getDefault().getPreferenceStore().setValue(PREF_CONFIGURATION_KEY,
                        configurationName);
        final ConfigurationContainer config = ConfigurationService
                        .getConfigurations(configurationName);
        for (CheckConfigurationContainer checker : config.getCheckConfigurations()) {
            Activator.getDefault().getPreferenceStore().setValue(
                            checker.getCheckId() + PREF_MAX_VALUE_KEY,
                            checker.getMaxValue().floatValue());
            Activator.getDefault().getPreferenceStore().setValue(
                            checker.getCheckId() + PREF_MIN_VALUE_KEY,
                            checker.getMinValue().floatValue());
            Activator.getDefault().getPreferenceStore().setValue(checker.getCheckId(),
                            checker.isEnabled());
        }
        UILogger.exiting(UserPreferencesService.class.getName(), method);

    }

    /**
     * Set back the active configuration to
     * {@link #PREF_CONFIGURATION_CUSTOMVALUE}.
     */
    public static void setDefaultConfiguration() {
        final String method = "setDefaultConfiguration";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        Activator.getDefault().getPreferenceStore().setToDefault(PREF_CONFIGURATION_KEY);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
    }

    /**
     * @return active configuration
     */
    public static String getConfigurationName() {
        final String method = "getConfigurationName";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().getString(PREF_CONFIGURATION_KEY);
    }

    /**
     * @return active configuration
     */
    public static boolean isDefaultConfigurationActive() {
        final String method = "isDefaultConfigurationActive";
        UILogger.entering(UserPreferencesService.class.getName(), method);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().getString(PREF_CONFIGURATION_KEY)
                        .equals(PREF_CONFIGURATION_CUSTOMVALUE);
    }

    /**
     * @param languageId
     *            that must be checked
     * @return whether or not the preference store contains this language.
     */
    public static boolean languageExists(String languageId) {
        final String method = "languageExists";
        UILogger.entering(UserPreferencesService.class.getName(), method, languageId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().contains(languageId);
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return the maxValue allowed for the <code>checkerId<code>, if it's set.
     */
    public static Float getMaxValue(String checkerId) {
        final String method = "getMaxValue";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Float.valueOf(Activator.getDefault().getPreferenceStore()
                        .getFloat(checkerId + PREF_MAX_VALUE_KEY));
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return whether or not a max value was set in preference for the
     *         <code>checkerId<code>.
     */
    public static boolean hasMaxValue(String checkerId) {
        final String method = "hasMaxValue";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().contains(checkerId + PREF_MAX_VALUE_KEY)
                        && !Float.isNaN(Activator.getDefault().getPreferenceStore()
                                        .getFloat(checkerId + PREF_MAX_VALUE_KEY));
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return the minimum value set for the <code>checkerId<code>, if one is
     *         set.
     */
    public static Float getMinValue(String checkerId) {
        final String method = "getMinValue";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Float.valueOf(Activator.getDefault().getPreferenceStore()
                        .getFloat(checkerId + PREF_MIN_VALUE_KEY));
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return whether or not a minimum value was set of this
     *         <code>checkedId</code>.
     */
    public static boolean hasMinValue(String checkerId) {
        final String method = "hasMinValue";
        UILogger.entering(UserPreferencesService.class.getName(), method, checkerId);
        UILogger.exiting(UserPreferencesService.class.getName(), method);
        return Activator.getDefault().getPreferenceStore().contains(checkerId + PREF_MIN_VALUE_KEY)
                        && !Float.isNaN(Activator.getDefault().getPreferenceStore()
                                        .getFloat(checkerId + PREF_MIN_VALUE_KEY));
    }

}
