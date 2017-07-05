package fr.cnes.analysis.tools.ui.preferences;

import fr.cnes.analysis.tools.ui.Activator;
import org.eclipse.jface.preference.IPreferenceStore;

public class CheckerPreferencesContainer {

    private String id;
    private String name;
    private boolean checked;
    private String severity;
    private Float maxValue;
    private Float minValue;
    private boolean isMetric;
    private String languageName;
    private String languageId;

    public CheckerPreferencesContainer(String languageId, String languageName, String id,
            String name, boolean checked, String severity, boolean pIsMetric) {
        this.languageId = languageId;
        this.languageName = languageName;
        this.id = id;
        this.name = name;
        this.checked = checked;
        this.severity = severity;
        this.minValue = Float.NaN;
        this.maxValue = Float.NaN;
        this.isMetric = pIsMetric;
    }

    public CheckerPreferencesContainer(String languageId, String languageName, String id,
            String name, boolean checked, String severity, Float minValue, Float maxValue,
            boolean pIsMetric) {
        this.languageId = languageId;
        this.languageName = languageName;
        this.id = id;
        this.name = name;
        this.checked = checked;
        this.severity = severity;
        this.minValue = minValue;
        this.maxValue = maxValue;
        this.isMetric = pIsMetric;

    }

    /**
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public final void setId(String id) {
        this.id = id;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public final void setName(String name) {
        this.name = name;
    }

    /**
     * @return the checked
     */
    public final boolean isChecked() {
        return checked;
    }

    /**
     * @param checked
     *            the checked to set
     */
    public final void setChecked(boolean checked) {
        this.checked = checked;
    }

    /**
     * @return the severity
     */
    public final String getSeverity() {
        return severity;
    }

    /**
     * @param severity
     *            the severity to set
     */
    public final void setSeverity(String severity) {
        this.severity = severity;
    }

    /**
     * @return the maxValue
     */
    public final Float getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public final void setMaxValue(float maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the minValue
     */
    public final Float getMinValue() {
        return minValue;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public final void setMinValue(float minValue) {
        this.minValue = minValue;
    }

    /**
     * @param languageId
     */
    public void savePreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.setValue(this.getId(), checked);
        store.setValue(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY, this.severity);
        if (!this.maxValue.isNaN()) {
            store.setValue(this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY, this.maxValue);
        }
        if (!this.minValue.isNaN()) {
            store.setValue(this.getId() + UserPreferencesService.PREF_MIN_VALUE_KEY, this.minValue);
        }
    }

    public void setToDefault() {
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.setToDefault(this.getId());
        store.setToDefault(this.getId() + UserPreferencesService.PREF_SEVERITY_KEY);
        if (this.isMetric) {
            store.setToDefault(this.getId() + UserPreferencesService.PREF_MAX_VALUE_KEY);
        }
        if (this.isMetric) {
            store.setToDefault(this.getId() + UserPreferencesService.PREF_MIN_VALUE_KEY);
        }
    }

    /**
     * @return the isMetric
     */
    public final boolean isMetric() {
        return isMetric;
    }

    /**
     * @param isMetric
     *            the isMetric to set
     */
    public final void setMetric(boolean isMetric) {
        this.isMetric = isMetric;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public final void setMaxValue(Float maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public final void setMinValue(Float minValue) {
        this.minValue = minValue;
    }

    /**
     * @return the languageId
     */
    public final String getLanguageId() {
        return languageId;
    }

    /**
     * @param languageId
     *            the languageId to set
     */
    public final void setLanguageId(String languageId) {
        this.languageId = languageId;
    }

    /**
     * @param languageName
     *            the languageName to set
     */
    public final void setLanguageName(String languageName) {
        this.languageName = languageName;
    }

    /**
     * @return the languageName
     */
    public final String getLanguageName() {
        return languageName;
    }

}
