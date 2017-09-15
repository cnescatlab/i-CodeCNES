/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.checkers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;

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
     * @return Every checkers contributing to the {@link IExtensionPoint}
     *         checks.
     * @throws NullContributionException
     *             When a checker's languageId refer to a language not
     *             contributing languages {@link IExtensionPoint}.
     * @throws CoreException
     *             When a plug-in can't be reached.
     */
    public static List<CheckerContainer> getCheckers()
                    throws NullContributionException, CoreException {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method);
        final List<CheckerContainer> checkers = new ArrayList<>();
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            // 1.1. Get the checker name
            final String checkerName = checkerElement
                            .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
            // 1.2. Get the checker id;
            final String checkerId = checkerElement
                            .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
            // 1.3 Get the Language thanks to the checker id;
            final LanguageContainer language = LanguageService.getLanguage(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
            // 1.4 Import the AbstractChecker class thanks
            final AbstractChecker checkerClass = (AbstractChecker) checkerElement
                            .createExecutableExtension(
                                            CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
            // 1.5 Is it a metric ? If yes then yes, otherwise no.
            final boolean isMetric;
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                isMetric = false;
            } else {
                isMetric = Boolean
                                .valueOf(checkerElement.getAttribute(
                                                CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC))
                                .booleanValue();
            }

            // 1.6 Create the Checker
            final CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                            checkerClass, checkerElement, isMetric);

            // 1.7 Add the checkers to all checkers.
            checkers.add(checker);
        }
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
     * @throws CoreException
     *             when for some runtime reason the resource couldn't be
     *             reached.
     */
    public static List<CheckerContainer> getCheckers(String languageId)
                    throws NullContributionException, CoreException {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, languageId);
        final List<CheckerContainer> checkers = new ArrayList<>();
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID)
                            .equals(languageId)) {
                // 1.1. Get the checker name
                final String checkerName = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                final String checkerId = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                final LanguageContainer language = LanguageService.getLanguage(checkerElement
                                .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                final AbstractChecker checkerClass = (AbstractChecker) checkerElement
                                .createExecutableExtension(
                                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Is it a metric ? If yes then yes, otherwise no.
                final boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean
                                    .valueOf(checkerElement.getAttribute(
                                                    CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC))
                                    .booleanValue();
                }

                // 1.6 Create the Checker
                final CheckerContainer checker = new CheckerContainer(checkerId, checkerName,
                                language, checkerClass, checkerElement, isMetric);

                // 1.7 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
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
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID)
                            .equals(languageId)) {
                checkers.add(checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID));
            }
        }
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
     * @throws CoreException
     *             when for some runtime reason the resource couldn't be
     *             reached.
     */
    public static List<CheckerContainer> getCheckers(List<String> checkersIds)
                    throws NullContributionException, CoreException {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, checkersIds);
        final List<CheckerContainer> checkers = new ArrayList<>();
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkersIds.contains(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID))) {
                // 1.1. Get the checker name
                final String checkerName = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                final String checkerId = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                final LanguageContainer language = LanguageService.getLanguage(checkerElement
                                .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                final AbstractChecker checkerClass = (AbstractChecker) checkerElement
                                .createExecutableExtension(
                                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Is it a metric ? If yes then yes, otherwise no.
                final boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean
                                    .valueOf(checkerElement.getAttribute(
                                                    CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC))
                                    .booleanValue();
                }

                // 1.6 Create the Checker
                final CheckerContainer checker = new CheckerContainer(checkerId, checkerName,
                                language, checkerClass, checkerElement, isMetric);

                // 1.6 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
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
     * @throws CoreException
     *             when for some runtime reason a contribution couldn't be
     *             reached.
     */
    public static List<CheckerContainer> getCheckers(List<String> languagesIds,
                    List<String> excludedIds) throws NullContributionException, CoreException {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, new Object[] {
            languagesIds, excludedIds
        });
        final List<CheckerContainer> checkers = new ArrayList<>();
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (!excludedIds.contains(checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID))
                            && languagesIds.contains(checkerElement.getAttribute(
                                            CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID))) {
                // 1.1.Get the checker's name;
                final String checkerName = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                final String checkerId = checkerElement
                                .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                final LanguageContainer language = LanguageService.getLanguage(checkerElement
                                .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                final AbstractChecker checkerClass = (AbstractChecker) checkerElement
                                .createExecutableExtension(
                                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Create the Checker
                final boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean
                                    .valueOf(checkerElement.getAttribute(
                                                    CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC))
                                    .booleanValue();
                }
                final CheckerContainer checker = new CheckerContainer(checkerId, checkerName,
                                language, checkerClass, checkerElement, isMetric);

                // 1.7 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
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
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID).equals(checkerId)) {
                isCheckerIdContributor = true;
            }
        }
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
     * @throws CoreException
     *             when for some runtime reason a contribution couldn't be
     *             reached.
     */
    public static CheckerContainer getChecker(String checkerId)
                    throws NullContributionException, CoreException {
        final String method = "getChecker";
        ICodeLogger.entering(CLASS, checkerId);
        final IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(CHECKER_EP_ID);
        boolean found = false;
        int checkerContributorsCounter = 0;
        IConfigurationElement checkerContributor;
        int checkerElementCounter = 0;
        IConfigurationElement checkerElement;
        CheckerContainer cherckerFound = null;
        while (checkerContributors.length > checkerContributorsCounter && !found) {
            checkerContributor = checkerContributors[checkerContributorsCounter];
            while (checkerContributor.getChildren(
                            CheckerService.CHECKER_EP_ELEMENT_CHECK).length > checkerElementCounter
                            && !found) {
                checkerElement = checkerContributor.getChildren(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK)[checkerElementCounter];
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID)
                                .equals(checkerId)) {
                    // 1.1. Get the checker name
                    final String checkerName = checkerElement
                                    .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                    // 1.3 Get the Language thanks to the checker id;
                    final LanguageContainer language = LanguageService.getLanguage(checkerElement
                                    .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                    // 1.4 Import the AbstractChecker class thanks
                    final AbstractChecker checkerClass = (AbstractChecker) checkerElement
                                    .createExecutableExtension(
                                                    CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);

                    // 1.5 Is it a metric ? If yes then yes, otherwise no.
                    final boolean isMetric;
                    if (checkerElement
                                    .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                        isMetric = false;
                    } else {
                        isMetric = Boolean
                                        .valueOf(checkerElement.getAttribute(
                                                        CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC))
                                        .booleanValue();
                    }
                    // 1.6 Create the Checker

                    cherckerFound = new CheckerContainer(checkerId, checkerName, language,
                                    checkerClass, checkerElement, isMetric);
                    found = true;

                }
                checkerElementCounter++;
            }
            checkerContributorsCounter++;
        }
        if (found) {
            ICodeLogger.exiting(CLASS, method, cherckerFound);
            return cherckerFound;
        }
        final NullContributionException exception = new NullContributionException(
                        "Impossible to find " + checkerId + " checker id in contributors.");
        ICodeLogger.throwing(CLASS, method, exception);
        throw exception;
    }

}
