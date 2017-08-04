/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import fr.cnes.analysis.tools.ui.Activator;

/**
 * Container for Checker preferences used by {@link UserPreferencesService}
 */
public class CheckerPreferencesContainer {

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
    public CheckerPreferencesContainer(String pLanguageId, String pLanguageName, String pId,
                    String pName, boolean pChecked, String pSeverity, boolean pIsMetric) {
        this.languageId = pLanguageId;
        this.languageName = pLanguageName;
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.severity = pSeverity;
        this.minValue = Float.valueOf(Float.NaN);
        this.maxValue = Float.valueOf(Float.NaN);
        this.isMetric = pIsMetric;
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
    public CheckerPreferencesContainer(String pLanguageId, String pLanguageName, String pId,
                    String pName, boolean pChecked, String pSeverity, Float pMinValue,
                    Float pMaxValue, boolean pIsMetric) {
        this.languageId = pLanguageId;
        this.languageName = pLanguageName;
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.severity = pSeverity;
        this.minValue = pMinValue;
        this.maxValue = pMaxValue;
        this.isMetric = pIsMetric;

    }

    /**
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param pId
     *            the id to set
     */
    public final void setId(String pId) {
        this.id = pId;
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
     * @return the checked
     */
    public final boolean isChecked() {
        return checked;
    }

    /**
     * @param pChecked
     *            the checked to set
     */
    public final void setChecked(boolean pChecked) {
        this.checked = pChecked;
    }

    /**
     * @return the severity
     */
    public final String getSeverity() {
        return severity;
    }

    /**
     * @param pSeverity
     *            the severity to set
     */
    public final void setSeverity(String pSeverity) {
        this.severity = pSeverity;
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        return maxValue;
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(float pMaxValue) {
        this.maxValue = Float.valueOf(pMaxValue);
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        return minValue;
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(float pMinValue) {
        this.minValue = Float.valueOf(pMinValue);
    }

    /**
     * @param languageId
     */
    public void savePreferences() {
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
    }

    /**
     * Set preferences to default.
     */
    public void setToDefault() {
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
    }

    /**
     * Update preferences store with current values of attributes.
     */
    public void update() {
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
    }

    /**
     * @return the isMetric
     */
    public final boolean isMetric() {
        return isMetric;
    }

    /**
     * @param pIsMetric
     *            the isMetric to set
     */
    public final void setMetric(boolean pIsMetric) {
        this.isMetric = pIsMetric;
    }

    /**
     * @param pMaxValue
     *            the maxValue to set
     */
    public final void setMaxValue(Float pMaxValue) {
        this.maxValue = pMaxValue;
    }

    /**
     * @param pMinValue
     *            the minValue to set
     */
    public final void setMinValue(Float pMinValue) {
        this.minValue = pMinValue;
    }

    /**
     * @return the languageId
     */
    public final String getLanguageId() {
        return languageId;
    }

    /**
     * @param pLanguageId
     *            the languageId to set
     */
    public final void setLanguageId(String pLanguageId) {
        this.languageId = pLanguageId;
    }

    /**
     * @param pLanguageName
     *            the languageName to set
     */
    public final void setLanguageName(String pLanguageName) {
        this.languageName = pLanguageName;
    }

    /**
     * @return the languageName
     */
    public final String getLanguageName() {
        return languageName;
    }

}
