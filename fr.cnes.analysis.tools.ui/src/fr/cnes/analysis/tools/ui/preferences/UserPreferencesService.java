package fr.cnes.analysis.tools.ui.preferences;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerContainer;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerService;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import fr.cnes.analysis.tools.ui.Activator;
import fr.cnes.analysis.tools.ui.configurations.CheckConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationService;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.PreferenceStore;

/**
 *
 */
public class UserPreferencesService extends AbstractPreferenceInitializer {

    public static final String PREF_SEVERITY_KEY = ".Severity";
    public static final String PREF_SEVERITY_ERROR_VALUE = "Error";
    public static final String PREF_SEVERITY_WARNING_VALUE = "Warning";
    public static final String PREF_SEVERITY_INFO_VALUE = "Info";
    public static final String PREF_MIN_VALUE_KEY = ".Min";
    public static final String PREF_MAX_VALUE_KEY = ".Max";
    public static final String PREF_CONFIGURATION_KEY = "Configuration";
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

        for (String languageId : LanguageService.getLanguagesIds()) {
            Activator.getDefault().getPreferenceStore().setDefault(languageId, true);
            for (CheckerContainer checker : CheckerService.getCheckers(languageId)) {
                Activator.getDefault().getPreferenceStore().setDefault(checker.getId(), true);
                Activator.getDefault().getPreferenceStore()
                        .setDefault(checker.getId() + PREF_SEVERITY_KEY, "Error");
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
     * initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        try {
            initPreferences();
        } catch (NullContributionException | CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * @param languageId
     *            Identifier of the language to enable.
     * @return Language enabling success.
     */
    public static boolean enableLanguage(String languageId) {
        boolean success = false;
        if (languageExists(languageId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, true);
            success = true;
        }
        return success;
    }

    /**
     * @param languageId
     *            Language identifier to verify.
     * @return Whether or not the language is enabled.
     */
    public static boolean isEnabledLanguage(String languageId) {
        return Activator.getDefault().getPreferenceStore().getBoolean(languageId);
    }

    /**
     * @param checkerId
     *            Identifier of the checker.
     * @return Whether or not the language is enabled.
     */
    public static boolean isEnabledChecker(String checkerId) {
        return Activator.getDefault().getPreferenceStore().getBoolean(checkerId);
    }

    /**
     * @param languageId
     *            Language identifier of language to disable.
     * @return Language disabling success.
     */
    public static boolean disableLanguage(String languageId) {
        boolean success = false;
        if (languageExists(languageId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, false);
            success = true;
        }
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
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(languageId, true);
            Activator.getDefault().getPreferenceStore().setValue(checkerId, true);
            success = true;
        }
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
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(checkerId, false);
            success = true;
        }
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
        final List<String> languages = new ArrayList<>();
        for (String language : LanguageService.getLanguagesIds()) {
            if (isEnabledLanguage(language)) {
                languages.add(language);
            }
        }
        return languages;
    }

    /**
     * @return identifiers of checkers disabled
     */
    public static List<String> getDisabledCheckersIds() {
        final List<String> checkers = new ArrayList<>();
        for (String languageId : getEnabledLanguagesIds()) {
            for (String checkerId : CheckerService.getCheckersIds(languageId)) {
                if (!isEnabledChecker(checkerId)) {
                    checkers.add(checkerId);
                }
            }
        }
        return checkers;
    }

    /**
     * This method verify and return for each languages contributing the
     * analyzer and it's checkers.
     * 
     * @return Language identifier of languages enabled in the
     *         {@link PreferenceStore}.
     * @throws CoreException
     * @throws NullContributionException
     */
    public static List<LanguagePreferencesContainer> getLanguagesPreferences()
            throws NullContributionException, CoreException {
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
                    minValue = Float.NaN;
                }
                final Float maxValue;
                if (hasMaxValue(checker.getId())) {
                    maxValue = getMaxValue(checker.getId());
                } else {
                    maxValue = Float.NaN;
                }
                checkPrefs.add(
                        new CheckerPreferencesContainer(languageId, languageName, checker.getId(),
                                checker.getName(), isEnabled, severity, minValue, maxValue, true));
            } else {
                checkPrefs.add(new CheckerPreferencesContainer(languageId, languageName,
                        checker.getId(), checker.getName(), isEnabled, severity, false));
            }
        }
        return checkPrefs;
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return severity of the checker
     */
    public static String getCheckerSeverity(String checkerId) {
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
        boolean success = false;
        if (checkerExists(languageId, checkerId)) {
            Activator.getDefault().getPreferenceStore().setValue(checkerId + PREF_SEVERITY_KEY,
                    severity);
            success = true;
        }
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
        return Activator.getDefault().getPreferenceStore().contains(checkerId);
    }

    /**
     * @param configurationName
     *            Configuration name to set in preferences.
     * @throws NullContributionException
     */
    public static void setConfiguration(String configurationName) throws NullContributionException {
        Activator.getDefault().getPreferenceStore().setValue(PREF_CONFIGURATION_KEY,
                configurationName);
        ConfigurationContainer config = ConfigurationService.getConfigurations(configurationName);
        for (CheckConfigurationContainer checker : config.getCheckConfigurations()) {
            Activator.getDefault().getPreferenceStore()
                    .setValue(checker.getCheckId() + PREF_MAX_VALUE_KEY, checker.getMaxValue());
            Activator.getDefault().getPreferenceStore()
                    .setValue(checker.getCheckId() + PREF_MIN_VALUE_KEY, checker.getMinValue());
            Activator.getDefault().getPreferenceStore().setValue(checker.getCheckId(),
                    checker.isEnabled());
        }

    }

    /**
     * Set back the active configuration to
     * {@link #PREF_CONFIGURATION_CUSTOMVALUE}.
     */
    public static void setDefaultConfiguration() {
        Activator.getDefault().getPreferenceStore().setToDefault(PREF_CONFIGURATION_KEY);
    }

    /**
     * @return active configuration
     */
    public static String getConfigurationName() {
        return Activator.getDefault().getPreferenceStore().getString(PREF_CONFIGURATION_KEY);
    }

    /**
     * @return active configuration
     */
    public static boolean isDefaultConfigurationActive() {
        return Activator.getDefault().getPreferenceStore().getString(PREF_CONFIGURATION_KEY)
                .equals(PREF_CONFIGURATION_CUSTOMVALUE);
    }

    /**
     * @param languageId
     *            that must be checked
     * @return whether or not the preference store contains this language.
     */
    public static boolean languageExists(String languageId) {
        return Activator.getDefault().getPreferenceStore().contains(languageId);
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return the maxValue allowed for the <code>checkerId<code>, if it's set.
     */
    public static Float getMaxValue(String checkerId) {
        return Activator.getDefault().getPreferenceStore().getFloat(checkerId + PREF_MAX_VALUE_KEY);
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return whether or not a max value was set in preference for the
     *         <code>checkerId<code>.
     */
    public static boolean hasMaxValue(String checkerId) {
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
        return Activator.getDefault().getPreferenceStore().getFloat(checkerId + PREF_MIN_VALUE_KEY);
    }

    /**
     * @param checkerId
     *            identifier of the checker
     * @return whether or not a minimum value was set of this
     *         <code>checkedId</code>.
     */
    public static boolean hasMinValue(String checkerId) {
        return Activator.getDefault().getPreferenceStore().contains(checkerId + PREF_MIN_VALUE_KEY)
                && !Float.isNaN(Activator.getDefault().getPreferenceStore()
                        .getFloat(checkerId + PREF_MIN_VALUE_KEY));
    }

}
