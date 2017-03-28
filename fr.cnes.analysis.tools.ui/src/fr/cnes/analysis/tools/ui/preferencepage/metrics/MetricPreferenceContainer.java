/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.metrics;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * This class is the preference container for metric preference pages. It
 * extends the preference container adding float value and level of criticity
 * 
 * 
 */
public class MetricPreferenceContainer {

    /** Id of the the preference **/
    private String metricId;

    /** Name of the preference **/
    private String metricName;

    /** Button to select the preference **/
    private Button metricCheckButton;

    /** Label **/
    private Text valueText;

    /**
     * Class constructor. This container is instantiated with metric id, metric
     * name, minimum, maximum and checked state.
     * 
     * @param pMetricId
     *            the metric id
     * @param pMetricName
     *            the metric name
     * @param pValueText
     *            the text value
     * @param pMetricCheckButton
     *            the check button
     */
    public MetricPreferenceContainer(final String pMetricId, final String pMetricName,
            final Text pValueText, final Button pMetricCheckButton) {
        this.metricId = pMetricId;
        this.metricName = pMetricName;
        this.metricCheckButton = pMetricCheckButton;
        this.valueText = pValueText;
    }

    /**
     * Store preferences into the preference store.
     * 
     * @param pCriticalLevel
     *            Level of critical development.
     */
    public void storePreference(String pCriticalLevel) {
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        store.setValue(metricId, metricCheckButton.getSelection());
        store.setValue(metricId + PreferencesUIUtils.VALUE + pCriticalLevel,
                Float.parseFloat(valueText.getText()));
    }

    /**
     * Set preferences to default.
     */
    public void setToDefault(String pCriticalLevel) {
        final IPreferenceStore store = PlatformUI.getPreferenceStore();

        store.setToDefault(metricId);
        store.setToDefault(metricId + PreferencesUIUtils.VALUE + pCriticalLevel);

        metricCheckButton.setSelection(store.getDefaultBoolean(metricId));
        valueText.setText(Float.toString(
                store.getDefaultFloat(metricId + PreferencesUIUtils.VALUE + pCriticalLevel)));
    }

    /**
     * @return the metricId
     */
    public String getMetricId() {
        return metricId;
    }

    /**
     * @param pMetricId
     *            the metricId to set
     */
    public void setMetricId(String pMetricId) {
        this.metricId = pMetricId;
    }

    /**
     * @return the metricName
     */
    public String getMetricName() {
        return metricName;
    }

    /**
     * @param pMetricName
     *            the metricName to set
     */
    public void setMetricName(String pMetricName) {
        this.metricName = pMetricName;
    }

    /**
     * @return the metricCheckButton
     */
    public Button getMetricCheckButton() {
        return metricCheckButton;
    }

    /**
     * @param pMetricCheckButton
     *            the metricCheckButton to set
     */
    public void setMetricCheckButton(Button pMetricCheckButton) {
        this.metricCheckButton = pMetricCheckButton;
    }

    /**
     * Retrieve the value of the container.
     * 
     * @return the value of the container
     */
    public Float getValue() {
        return Float.parseFloat(valueText.getText());
    }

    /**
     * Set the value of the container.
     * 
     * @param pValue
     *            the value.
     */
    public void setValue(final Float pValue) {
        valueText.setText(pValue.toString());
    }

    public void setEnabled(boolean enabled) {
        valueText.setEnabled(enabled);
    }

}
