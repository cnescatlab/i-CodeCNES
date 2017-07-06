package fr.cnes.analysis.tools.analyzer.services.checkers;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;

/**
 *
 *
 * @since 3.0
 */
public class CheckerService {

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
        List<CheckerContainer> checkers = new ArrayList<>();
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            // 1.1. Get the checker name
            String checkerName = checkerElement
                    .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
            // 1.2. Get the checker id;
            String checkerId = checkerElement
                    .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
            // 1.3 Get the Language thanks to the checker id;
            LanguageContainer language = LanguageService.getLanguage(
                    checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
            // 1.4 Import the AbstractChecker class thanks
            AbstractChecker checkerClass = (AbstractChecker) checkerElement
                    .createExecutableExtension(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
            // 1.5 Is it a metric ? If yes then yes, otherwise no.
            boolean isMetric;
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                isMetric = false;
            } else {
                isMetric = Boolean.valueOf(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC));
            }

            // 1.6 Create the Checker
            CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                    checkerClass, checkerElement, isMetric);

            // 1.7 Add the checkers to all checkers.
            checkers.add(checker);
        }
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
        List<CheckerContainer> checkers = new ArrayList<>();
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID)
                    .equals(languageId)) {
                // 1.1. Get the checker name
                String checkerName = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                String checkerId = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                LanguageContainer language = LanguageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Is it a metric ? If yes then yes, otherwise no.
                boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean.valueOf(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC));
                }

                // 1.6 Create the Checker
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement, isMetric);

                // 1.7 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
        return checkers;
    }

    /**
     * @param languageId
     *            the language identifier to retrieve checkers from.
     * @return A list of every checker's ids referencing languageId.
     */
    public static List<String> getCheckersIds(String languageId) {
        List<String> checkers = new ArrayList<>();
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID)
                    .equals(languageId)) {
                checkers.add(checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID));
            }
        }
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
        List<CheckerContainer> checkers = new ArrayList<>();
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkersIds
                    .contains(checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID))) {
                // 1.1. Get the checker name
                String checkerName = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                String checkerId = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                LanguageContainer language = LanguageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Is it a metric ? If yes then yes, otherwise no.
                boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean.valueOf(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC));
                }

                // 1.6 Create the Checker
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement, isMetric);

                // 1.6 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
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
        List<CheckerContainer> checkers = new ArrayList<>();
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (!excludedIds.contains(checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID))
                    && languagesIds.contains(checkerElement.getAttribute(
                            CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID))) {
                // 1.1.Get the checker's name;
                String checkerName = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                // 1.2. Get the checker id;
                String checkerId = checkerElement
                        .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                // 1.3 Get the Language thanks to the checker id;
                LanguageContainer language = LanguageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Create the Checker
                boolean isMetric;
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                    isMetric = false;
                } else {
                    isMetric = Boolean.valueOf(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC));
                }
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement, isMetric);

                // 1.7 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }

        return checkers;

    }

    /**
     * @param checkerId
     *            checker identifier to verify contribution
     * @return whether or not the checker identifier in <code>checkerId</code>
     *         parameter is contributing to {@link #CHECKER_EP_ID}.
     */
    public static boolean isCheckerIdContributor(String checkerId) {
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID).equals(checkerId)) {
                return true;
            }
        }
        return false;
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
     */
    public static CheckerContainer getChecker(String checkerId)
            throws NullContributionException, CoreException {
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerContributor : checkerContributors) {
            for (IConfigurationElement checkerElement : checkerContributor
                    .getChildren(CheckerService.CHECKER_EP_ELEMENT_CHECK)) {
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID)
                        .equals(checkerId)) {
                    // 1.1. Get the checker name
                    String checkerName = checkerElement
                            .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                    // 1.3 Get the Language thanks to the checker id;
                    LanguageContainer language = LanguageService.getLanguage(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                    // 1.4 Import the AbstractChecker class thanks
                    AbstractChecker checkerClass = (AbstractChecker) checkerElement
                            .createExecutableExtension(
                                    CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);

                    // 1.5 Is it a metric ? If yes then yes, otherwise no.
                    boolean isMetric;
                    if (checkerElement
                            .getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC) == null) {
                        isMetric = false;
                    } else {
                        isMetric = Boolean.valueOf(
                                checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ISMETRIC));
                    }
                    // 1.6 Create the Checker

                    return new CheckerContainer(checkerId, checkerName, language, checkerClass,
                            checkerElement, isMetric);

                }
            }
        }
        throw new NullContributionException(
                "Impossible to find " + checkerId + " checker id in contributors.");
    }

}
