/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

/**
 *
 */
public class CheckersFilter extends ViewerFilter {

    private String searchString;

    public void setSearchText(String s) {
        // ensure that the value can be used for matching
        this.searchString = "(?i:.*" + s + ".*)";
    }

    @Override
    public boolean select(Viewer viewer, Object parentElement, Object element) {
        if (searchString == null || searchString.length() == 0) {
            return true;
        }
        CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
        if (checker.getName().matches(searchString)
                || checker.getLanguageName().matches(searchString)) {
            return true;
        }

        return false;
    }

}
