/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.Activator;

/**
 * Container for Checker preferences used by {@link UserPreferencesService}
 */
public class CheckerPreferencesContainer {

    /** Class name */
    private static final String CLASS = CheckerPreferencesContainer.class.getName();

    /** Checker's identifier */
    private String id;
    /** Checker's name */
    private String name;
    /** Checker's enabling */
    private boolean checked;
    /** Checker's severity */
    private String severity;
    /** Checker's max value */
    private Float maxValue;
    /** Checker's min value */
    private Float minValue;
    /** Checker is a metric */
    private boolean isMetric;
    /** Checker language's name */
    private String languageName;
    /** Checker language's id */
    private String languageId;

    /**
     * @param pLanguageId
     *            Checker language's id
     * @param pLanguageName
     *            Checker language's name
     * @param pId
     *            Checker's identifier
     * @param pName
     *            Checker's name
     * @param pChecked
     *            Checker's enabling
     * @param pSeverity
     *            Checker's severity
     * @param pIsMetric
     *            Checker is a metric
     */
    public CheckerPreferencesContainer(final String pLanguageId, final String pLanguageName,
                    final String pId, final String pName, final boolean pChecked,
                    final String pSeverity, final boolean pIsMetric) {
        final String method = "CheckerPreferencesContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pLanguageId, pLanguageName, pId, pName, Boolean.valueOf(pChecked), pSeverity,
            Boolean.valueOf(pIsMetric)
        });
        this.languageId = pLanguageId;
        this.languageName = pLanguageName;
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.severity = pSeverity;
        this.minValue = Float.valueOf(Float.NaN);
        this.maxValue = Float.valueOf(Float.NaN);
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pLanguageId
     *            Checker language's id
     * @param pLanguageName
     *            Checker language's name
     * @param pId
     *            Checker's identifier
     * @param pName
     *            Checker's name
     * @param pChecked
     *            Checker's enabling
     * @param pSeverity
     *            Checker's severity
     * @param pMinValue
     *            Checker's min value
     * @param pMaxValue
     *            Checker's max value
     * @param pIsMetric
     *            Checker is a metric
     */
    public CheckerPreferencesContainer(final String pLanguageId, final String pLanguageName,
                    final String pId, final String pName, final boolean pChecked,
                    final String pSeverity, final Float pMinValue, final Float pMaxValue,
                    final boolean pIsMetric) {
        final String method = "CheckerPreferencesContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pLanguageId, pLanguageName, pId, pName, Boolean.valueOf(pChecked), pSeverity, pMinValue,
            pMaxValue, Boolean.valueOf(pIsMetric)
        });
        this.languageId = pLanguageId;
        this.languageName = pLanguageName;
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.severity = pSeverity;
        this.minValue = pMinValue;
        this.maxValue = pMaxValue;
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);

    }

    /**
     * @return the id
     */
    public final String getId() {
        final String method = "getId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, id);
        return id;
    }

    /**
     * @param pId
     *            the id to set
     */
    public final void setId(final String pId) {
        final String method = "setId";
        ICodeLogger.entering(CLASS, method, pId);
        this.id = pId;
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
    public final void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the checked
     */
    public final boolean isChecked() {
        final String method = "isChecked";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(checked));
        return checked;
    }

    /**
     * @param pChecked
     *            the checked to set
     */
    public final void setChecked(final boolean pChecked) {
        final String method = "setChecked";
        ICodeLogger.entering(CLASS, method, Boolean.valueOf(pChecked));
        this.checked = pChecked;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the severity
     */
    public final String getSeverity() {
        final String method = "getSeverity";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, severity);
        return severity;
    }

    /**
     * @param pSeverity
     *            the severity to set
     */
    public final void setSeverity(final String pSeverity) {
        final String method = "setSeverity";
        ICodeLogger.entering(CLASS, method, pSeverity);
        this.severity = pSeverity;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        final String method = "getMaxValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, maxValue);
        return maxValue;
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(final float pMaxValue) {
        final String method = "setMaxValue";
        ICodeLogger.entering(CLASS, method);
        this.maxValue = Float.valueOf(pMaxValue);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        final String method = "getMinValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, minValue);
        return minValue;
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(final float pMinValue) {
        final String method = "setMinValue";
        ICodeLogger.entering(CLASS, method, Float.valueOf(pMinValue));
        this.minValue = Float.valueOf(pMinValue);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param languageId
     */
    public void savePreferences() {
        final String method = "savePreferences";
        ICodeLogger.entering(CLASS, method);
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.setValue(this.getId(), checked);
        store.setValue(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY, this.severity);
        if (!this.maxValue.isNaN()) {
            store.setValue(this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY,
                            this.maxValue.floatValue());
        }
        if (!this.minValue.isNaN()) {
            store.setValue(this.getId() + UserPreferencesService.PREF_MIN_VALUE_KEY,
                            this.minValue.floatValue());
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Set preferences to default.
     */
    public void setToDefault() {
        final String method = "setToDefault";
        ICodeLogger.entering(CLASS, method);
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        this.checked = store.getDefaultBoolean(this.getId());
        store.setToDefault(this.getId());
        this.severity = store
                        .getDefaultString(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY);
        store.setToDefault(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY);

        if (this.isMetric) {
            this.maxValue = Float.valueOf(store.getDefaultFloat(
                            this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY));
            store.setToDefault(this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY);
        }
        if (this.isMetric) {
            this.minValue = Float.valueOf(store.getDefaultFloat(
                            this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY));
            store.setToDefault(this.getId() + UserPreferencesService.PREF_MIN_VALUE_KEY);
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Update preferences store with current values of attributes.
     */
    public void update() {
        final String method = "update";
        ICodeLogger.entering(CLASS, method);
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        if (!store.contains(this.getId())) {
            this.checked = true;
        } else {
            this.checked = store.getBoolean(this.getId());
        }
        if (!store.contains(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY)) {
            this.severity = UserPreferencesService.PREF_SEVERITY_ERROR_VALUE;
        } else {
            this.severity = store
                            .getString(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY);
        }

        if (this.isMetric) {
            this.maxValue = Float.valueOf(store
                            .getFloat(this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY));
        }
        if (this.isMetric) {
            this.minValue = Float.valueOf(store
                            .getFloat(this.getId() + UserPreferencesService.PREF_MIN_VALUE_KEY));
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the isMetric
     */
    public final boolean isMetric() {
        final String method = "isMetric";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(isMetric));
        return isMetric;
    }

    /**
     * @param pIsMetric
     *            the isMetric to set
     */
    public final void setMetric(final boolean pIsMetric) {
        final String method = "setMetric";
        ICodeLogger.entering(CLASS, method, Boolean.valueOf(pIsMetric));
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(final Float pMaxValue) {
        final String method = "setMaxValue";
        ICodeLogger.entering(CLASS, method, pMaxValue);
        this.maxValue = pMaxValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(final Float pMinValue) {
        final String method = "setMinValue";
        ICodeLogger.entering(CLASS, method, pMinValue);
        this.minValue = pMinValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the languageId
     */
    public final String getLanguageId() {
        final String method = "getLanguageId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, languageId);
        return languageId;
    }

    /**
     * @param pLanguageId
     *            the languageId to set
     */
    public final void setLanguageId(final String pLanguageId) {
        final String method = "setLanguageId";
        ICodeLogger.entering(CLASS, method, pLanguageId);
        this.languageId = pLanguageId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pLanguageName
     *            the languageName to set
     */
    public final void setLanguageName(final String pLanguageName) {
        final String method = "setLanguageName";
        ICodeLogger.entering(CLASS, method, pLanguageName);
        this.languageName = pLanguageName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the languageName
     */
    public final String getLanguageName() {
        final String method = "getLanguageName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, languageName);
        return languageName;
    }

}
