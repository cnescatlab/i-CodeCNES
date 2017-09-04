/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.logger.UILogger;
import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;

/**
 * Filter for {@link CheckerPreferencesContainer}
 */
public class CheckersFilter extends ViewerFilter {

    /** Class name **/
    private static final String CLASS = CheckersFilter.class.getName();

    /** Search string */
    private String searchString;

    /**
     * Adding pattern to <code>s</code> parameter.
     * 
     * @param str
     *            Searched string.
     */
    public void setSearchText(final String str) {
        final String method = "setSearchText";
        UILogger.entering(CLASS, method, str);
        // ensure that the value can be used for matching
        this.searchString = "(?i:.*" + str + ".*)";
        UILogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.
     * Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
        final String method = "select";
        UILogger.entering(CLASS, method, new Object[] {
            viewer, parentElement, element
        });
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

        UILogger.exiting(CLASS, method, Boolean.valueOf(select));

        return select;
    }

}
