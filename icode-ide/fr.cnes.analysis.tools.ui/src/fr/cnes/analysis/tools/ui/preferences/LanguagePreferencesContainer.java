/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import java.util.ArrayList;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.Activator;

/**
 * Container for languages used {@link UserPreferencesService}
 */
public class LanguagePreferencesContainer {

    /** Class name */
    private static final String CLASS = LanguagePreferencesContainer.class.getName();

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
    public LanguagePreferencesContainer(final String pId, final boolean pChecked) {
        final String method = "LanguagePreferencesContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, Boolean.valueOf(pChecked)
        });
        this.id = pId;
        this.checked = pChecked;
        this.checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pId
     *            Language's identifier
     * @param pName
     *            Language's name
     * @param pChecked
     *            Language selection
     */
    public LanguagePreferencesContainer(final String pId, final String pName,
                    final boolean pChecked) {
        final String method = "LanguagePreferencesContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, pName, Boolean.valueOf(pChecked)
        });
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method);
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
    public LanguagePreferencesContainer(final String pId, final String pName,
                    final boolean pChecked, final List<CheckerPreferencesContainer> pCheckers) {
        final String method = "LanguagePreferencesContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, pName, Boolean.valueOf(pChecked), pCheckers
        });
        this.id = pId;
        this.name = pName;
        this.checked = pChecked;
        this.checkers = pCheckers;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pChecked
     *            selection to set.
     */
    public void setChecked(final Boolean pChecked) {
        final String method = "setChecked";
        ICodeLogger.entering(CLASS, method, pChecked);
        checked = pChecked.booleanValue();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * This method set checked to the default value found in the preference
     * store. It also set the preference store value to default. This method
     * uses setChecked method, thus the button is also updated.
     */
    public void setToDefault() {
        final String method = "setToDefault";
        ICodeLogger.entering(CLASS, method);
        Activator.getDefault().getPreferenceStore().setToDefault(id);
        for (final CheckerPreferencesContainer checker : checkers) {
            checker.setToDefault();
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * This method save preferences in i-Code UI preferences stores with
     * containers values.
     */
    public void savePreferences() {
        final String method = "savePreferences";
        ICodeLogger.entering(CLASS, method);
        for (final CheckerPreferencesContainer checker : checkers) {
            checker.savePreferences();
        }
        Activator.getDefault().getPreferenceStore().setValue(id, checked);
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
     * @return the checkers
     */
    public final List<CheckerPreferencesContainer> getCheckers() {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param pCheckers
     *            the checkers to set
     */
    public final void setCheckers(final List<CheckerPreferencesContainer> pCheckers) {
        final String method = "setCheckers";
        ICodeLogger.entering(CLASS, method, pCheckers);
        this.checkers = pCheckers;
        ICodeLogger.exiting(CLASS, method);
    }

}
