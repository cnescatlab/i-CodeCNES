/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.rules;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Button;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * This class is the preference container for rule preference pages. It extends
 * the preference container adding string value (criticity).
 * 
 * 
 */
public class RulePreferenceContainer {

    /** Id of the the preference **/
    private String ruleId;

    /** Name of the preference **/
    private String ruleName;

    /** Button to select the preference **/
    private Button ruleCheckButton;

    /**
     * A boolean which is true when the preference is selected, false otherwise
     **/
    private transient Boolean checked;

    /** Rule criticity **/
    private String ruleCriticity;

    /**
     * Class constructor. This class is instantiated with rule id, rule name,
     * rule criticity and checked state.
     * 
     * @param pRuleId
     *            id of the rule
     * @param pRuleName
     *            name of the rule
     * @param pCriticity
     *            criticity of the rule
     * @param pChecked
     *            true if selected, false otherwise
     */
    public RulePreferenceContainer(final String pRuleId, final String pRuleName,
            final String pCriticity, final Boolean pChecked) {
        this.ruleId = pRuleId;
        this.ruleName = pRuleName;
        this.checked = pChecked;
        this.ruleCriticity = pCriticity;
    }

    /**
     * Getter for the criticity of the rule.
     * 
     * @return rule's criticity
     */
    public String getRuleCriticity() {
        return this.ruleCriticity;
    }

    /**
     * Setter for the rule's criticity.
     * 
     * @param pCriticity
     *            the rule's criticity to set
     */
    public void setRuleCriticity(final String pCriticity) {
        this.ruleCriticity = pCriticity;
    }

    /**
     * Store rule preferences into the preference store.
     */
    public void storePreference() {
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        store.setValue(ruleId, checked);
        store.setValue(this.getRuleId() + PreferencesUIUtils.CRITICAL, this.ruleCriticity);
    }

    /**
     * Set rule preference to default value.
     */
    public void setToDefault() {
        this.setChecked(PlatformUI.getPreferenceStore().getDefaultBoolean(this.ruleId));
        PlatformUI.getPreferenceStore().setToDefault(this.ruleId);

        final IPreferenceStore store = PlatformUI.getPreferenceStore();

        // We also set the critical to default
        this.setRuleCriticity(
                store.getDefaultString(this.getRuleId() + PreferencesUIUtils.CRITICAL));
        store.setToDefault(this.getRuleId() + PreferencesUIUtils.CRITICAL);

        this.ruleCheckButton.setSelection(this.isChecked());
    }

    /**
     * @return the ruleId
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * @param pRuleId
     *            the ruleId to set
     */
    public void setRuleId(String pRuleId) {
        this.ruleId = pRuleId;
    }

    /**
     * @return the ruleName
     */
    public String getRuleName() {
        return ruleName;
    }

    /**
     * @param pRuleName
     *            the ruleName to set
     */
    public void setRuleName(String pRuleName) {
        this.ruleName = pRuleName;
    }

    /**
     * @return the ruleCheckButton
     */
    public Button getRuleCheckButton() {
        return ruleCheckButton;
    }

    /**
     * @param pRuleCheckButton
     *            the ruleCheckButton to set
     */
    public void setRuleCheckButton(Button pRuleCheckButton) {
        this.ruleCheckButton = pRuleCheckButton;
    }

    /**
     * @return the checked
     */
    public Boolean isChecked() {
        return checked;
    }

    /**
     * @param pChecked
     *            the checked to set
     */
    public void setChecked(Boolean pChecked) {
        this.checked = pChecked;
    }

}