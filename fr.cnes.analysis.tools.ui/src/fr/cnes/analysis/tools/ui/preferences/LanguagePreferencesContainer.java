/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import java.util.ArrayList;
import java.util.List;

import fr.cnes.analysis.tools.ui.Activator;

/**
 * Container for languages used {@link UserPreferencesService}
 */
public class LanguagePreferencesContainer {

    /** Language's identifier */
    private String id;
    /** Language's name */
    private String name;
    /** Language selection */
    private boolean checked;
    /** Language's checkers */
    private List<CheckerPreferencesContainer> checkers;

    /**
     * @param pId
     *            Language's identifier
     * @param pChecked
     *            Language's checkers
     */
    public LanguagePreferencesContainer(String pId, boolean pChecked) {
        this.id = pId;
        this.checked = pChecked;
        this.checkers = new ArrayList<>();
    }

    /**
     * @param pId
     *            Language's identifier
     * @param pName
     *            Language's name
     * @param pChecked
     *            Language selection
     */
    public LanguagePreferencesContainer(String pId, String pName, boolean pChecked) {
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.checkers = new ArrayList<>();
    }

    /**
     * @param pId
     *            Language's identifier
     * @param pName
     *            Language's name
     * @param pChecked
     *            Language selection
     * @param pCheckers
     *            Language's checkers
     */
    public LanguagePreferencesContainer(String pId, String pName, boolean pChecked,
                    List<CheckerPreferencesContainer> pCheckers) {
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.checkers = pCheckers;
    }

    /**
     * @param pChecked
     *            selection to set.
     */
    public void setChecked(final Boolean pChecked) {
        checked = pChecked.booleanValue();
    }

    /**
     * This method set checked to the default value found in the preference
     * store. It also set the preference store value to default. This method
     * uses setChecked method, thus the button is also updated.
     */
    public void setToDefault() {
        Activator.getDefault().getPreferenceStore().setToDefault(id);
        for (CheckerPreferencesContainer checker : checkers) {
            checker.setToDefault();
        }
    }

    /**
     * This method save preferences in i-Code UI preferences stores with
     * containers values.
     */
    public void savePreferences() {
        for (CheckerPreferencesContainer checker : checkers) {
            checker.savePreferences();
        }
        Activator.getDefault().getPreferenceStore().setValue(id, checked);
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
     * @return the checkers
     */
    public final List<CheckerPreferencesContainer> getCheckers() {
        return checkers;
    }

    /**
     * @param pCheckers
     *            the checkers to set
     */
    public final void setCheckers(List<CheckerPreferencesContainer> pCheckers) {
        this.checkers = pCheckers;
    }

}
