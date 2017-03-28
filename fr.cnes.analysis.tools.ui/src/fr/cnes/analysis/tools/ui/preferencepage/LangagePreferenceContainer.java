/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage;

import org.eclipse.swt.widgets.Button;
import org.eclipse.ui.PlatformUI;

/**
 * This class is used to store preferences. It can represent a enabling language
 * metric or rule, enabling language analysis
 * 
 */
public class LangagePreferenceContainer {
    /** Id of the the preference **/
    private String preferenceId;

    /** Name of the preference **/
    private String name;

    /** Button to select the preference **/
    private Button button;
    /**
     * A boolean which is true when the preference is selected, false otherwise
     **/
    private transient Boolean checked;

    /**
     * Class constructor. To instantiate this class, preference id, preference
     * name and checked state are needed. The button is instantiated to null. It
     * should be set before using this class.
     * 
     * @param pPrefId
     *            the preference id
     * @param pName
     *            the preference name
     * @param pChecked
     *            true if the preference is selected, false otherwise
     */
    public LangagePreferenceContainer(final String pPrefId, final String pName,
            final Boolean pChecked) {
        this.preferenceId = pPrefId;
        this.name = pName;
        this.checked = pChecked;
    }

    /**
     * Getter for the preference id.
     * 
     * @return the preference id
     */
    public String getPrefId() {
        return preferenceId;
    }

    /**
     * Getter for the preference name.
     * 
     * @return the preference name
     */
    public String getName() {
        return name;
    }

    /**
     * Getter for the button linked to the preference.
     * 
     * @return the button
     */
    public Button getButton() {
        return button;
    }

    /**
     * Get for checked variable.
     * 
     * @return true if the preference is selected, false otherwise
     */
    public Boolean isChecked() {
        return checked;
    }

    /**
     * Setter for the preference id.
     * 
     * @param pPreferenceId
     *            the preference id to set
     */
    public void setPrefId(final String pPreferenceId) {
        this.preferenceId = pPreferenceId;
    }

    /**
     * Setter for the preference name.
     * 
     * @param pName
     *            the preference name to set
     */
    public void setName(final String pName) {
        this.name = pName;
    }

    /**
     * Setter for the button.
     * 
     * @param pButton
     *            the button to set
     */
    public void setButton(final Button pButton) {
        this.button = pButton;
    }

    /**
     * Setter for the checked state. It also updates the button if it exists.
     * 
     * @param pChecked
     *            true if selected, false otherwise
     */
    public void setChecked(final Boolean pChecked) {
        checked = pChecked;

        // If there is a button linked to the preference, it is updated
        // considering the new value of checked.
        if (button != null) {
            button.setSelection(pChecked);
        }

    }

    /**
     * This method stores the preference into the preference store.
     */
    public void storePreference() {
        setChecked(button.getSelection());
        PlatformUI.getPreferenceStore().setValue(preferenceId, checked);
    }

    /**
     * This method set checked to the default value found in the preference
     * store. It also set the preference store value to default. This method
     * uses setChecked method, thus the button is also updated.
     */
    public void setToDefault() {
        setChecked(PlatformUI.getPreferenceStore().getDefaultBoolean(preferenceId));
        PlatformUI.getPreferenceStore().setToDefault(preferenceId);
    }
}
