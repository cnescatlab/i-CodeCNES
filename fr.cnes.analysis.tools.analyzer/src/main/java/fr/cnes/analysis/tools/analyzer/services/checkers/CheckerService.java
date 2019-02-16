/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.checkers;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;

import java.util.ArrayList;
import java.util.List;

/**
 * This service can be used to reach Checker available in
 * {@link #CHECKER_EP_ID}. Some function of this service returns Checker in
 * {@link CheckerContainer}.
 *
 * @since 3.0
 */
public final class CheckerService {

    /** Extension Point ID . */
    public static final String CHECKER_EP_ID = "fr.cnes.analysis.tools.analyzer.checks";
    /** Checker extension point's name. */
    public static final String CHECKER_EP_NAME = "checks";
    /** Check element key */
    public static final String CHECKER_EP_ELEMENT_CHECK = "check";
    /** Check element's name attribute key */
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_NAME = "name";
    /** Check's element identifier attribute key */
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_ID = "id";
    /**
     * Check's element language identifier attribute (in reference to a
     * languageId contributing
     */
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID = "languageId";
    /** Check's element class attribute key */
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_CLASS = "class";
    /** Check's element isMetric attribute key */
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC = "isMetric";

    /** Class name **/
    private static final String CLASS = CheckerService.class.getName();

    /**
     * Utils class default constructor removal.
     */
    private CheckerService() {
        // Nothing here.
    }

    /**
     * @return Every checkers contributing to the extension point checks.
     */
    public static List<CheckerContainer> getCheckers() {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method);
        final List<CheckerContainer> checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param languageId
     *            to retrieve checker's from.
     * @return a list of {@link CheckerContainer} contributing to
     *         {@link #CHECKER_EP_ID} and referencing <code>languageId</code>
     *         parameter.
     * @throws NullContributionException
     *             when a contribution do not exist, it can be both due to :
     *             <ul>
     *             <li>the <code>languageId</code> parameter is not defined in
     *             the {@link LanguageService#LANGUAGE_EP_ID} extension
     *             point;</li>
     *             <li>a checker contributing to the extension point is
     *             referencing a languageId not existing</li>
     *             </ul>
     */
    public static List<CheckerContainer> getCheckers(String languageId) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, languageId);
        final List<CheckerContainer> checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param languageId
     *            the language identifier to retrieve checkers from.
     * @return A list of every checker's ids referencing languageId.
     */
    public static List<String> getCheckersIds(String languageId) {
        final String method = "getCheckersIds";
        ICodeLogger.entering(CLASS, method, languageId);
        final List<String> checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param checkersIds
     *            list of checkers identifiers to retrieve
     *            {@link CheckerContainer} class from.
     * @return A list of {@link CheckerContainer} for each checker identifier in
     *         <code>checkersIds</code>.
     * @throws NullContributionException
     *             when a contribution couldn't be reached for one of these
     *             reasons :
     *             <ul>
     *             <li>One of the checker identifier of <code>checkersIds</code>
     *             isn't contributing to {@link #CHECKER_EP_ID};</li>
     *             <li>One of the language identifier referenced by one of the
     *             checker in <code>checkersIds</code> couldn't be reached.</li>
     *             </ul>
     */
    public static List<CheckerContainer> getCheckers(List<String> checkersIds) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, checkersIds);
        final List<CheckerContainer> checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param languagesIds
     *            language identifier to retrieve checkers from.
     * @param excludedIds
     *            checkers identifier to not include in the returned checkers.
     * @return a list of {@link CheckerContainer} containing all checkers
     *         referencing a language identifier in <code>languagesIds</code>
     *         except the ones contained in <code>excludedIds</code>.
     * @throws NullContributionException
     *             when a contribution coudln't be reached because the language
     *             identifier, the checker identifier, or the language
     *             identifier referenced by the checker contribution couldn't be
     *             reached.
     */
    public static List<CheckerContainer> getCheckers(List<String> languagesIds,
                    List<String> excludedIds) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, new Object[] {
            languagesIds, excludedIds
        });
        final List<CheckerContainer> checkers = new ArrayList<>();
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param checkerId
     *            checker identifier to verify contribution
     * @return whether or not the checker identifier in <code>checkerId</code>
     *         parameter is contributing to {@link #CHECKER_EP_ID}.
     */
    public static boolean isCheckerIdContributor(String checkerId) {
        final String method = "isCheckerIdContributor";
        ICodeLogger.entering(CLASS, method, checkerId);
        boolean isCheckerIdContributor = false;
        ICodeLogger.exiting(CLASS, method);
        return isCheckerIdContributor;
    }

    /**
     * @param checkerId
     *            checker identifier to return {@link CheckerContainer} from.
     * @return A {@link CheckerContainer} based on <code>checkerId</code> in
     *         paramater.
     * @throws NullContributionException
     *             when <code>checkerId</code> do not contribute to
     *             {@link #CHECKER_EP_ID} or that languageId referenced by
     *             <code>checkerId</code>'s contribution is not contributing to
     *             {@link LanguageService#LANGUAGE_EP_ID}.
     */
    public static CheckerContainer getChecker(String checkerId) {
        final String method = "getChecker";
        ICodeLogger.entering(CLASS, checkerId);
        ICodeLogger.exiting(CLASS, method, null /* checker found here */);
        return null;
    }

}
