package fr.cnes.analysis.tools.ui.preferences;

import fr.cnes.analysis.tools.ui.Activator;
import java.util.ArrayList;
import java.util.List;

public class LanguagePreferencesContainer {

    private String id;
    private String name;
    private boolean checked;

    private List<CheckerPreferencesContainer> checkers;

    public LanguagePreferencesContainer(String id, boolean checked) {
        this.id = id;
        this.checked = checked;
        this.checkers = new ArrayList<>();
    }

    public LanguagePreferencesContainer(String id, String name, boolean checked) {
        this.id = id;
        this.name = name;
        this.checked = checked;
        this.checkers = new ArrayList<>();
    }

    public LanguagePreferencesContainer(String id, String name, boolean checked,
            List<CheckerPreferencesContainer> pCheckers) {
        this.id = id;
        this.name = name;
        this.checked = checked;
        this.checkers = pCheckers;
    }

    public void setChecked(final Boolean pChecked) {
        checked = pChecked;
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
     * @return the checkers
     */
    public final List<CheckerPreferencesContainer> getCheckers() {
        return checkers;
    }

    /**
     * @param checkers
     *            the checkers to set
     */
    public final void setCheckers(List<CheckerPreferencesContainer> checkers) {
        this.checkers = checkers;
    }

}
