/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;

/**
 * Filter for {@link CheckerPreferencesContainer}
 */
public class CheckersFilter extends ViewerFilter {

    /** Search string */
    private String searchString;

    /**
     * Adding pattern to <code>s</code> parameter.
     * 
     * @param s
     *            Searched string.
     */
    public void setSearchText(String s) {
        // ensure that the value can be used for matching
        this.searchString = "(?i:.*" + s + ".*)";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.
     * Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean select(Viewer viewer, Object parentElement, Object element) {
        boolean select = false;
        if (searchString == null || searchString.length() == 0) {
            select = true;
        } else {
            final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
            if (checker.getName().matches(searchString)
                            || checker.getLanguageName().matches(searchString)) {
                select = true;
            }
        }

        return select;
    }

}
