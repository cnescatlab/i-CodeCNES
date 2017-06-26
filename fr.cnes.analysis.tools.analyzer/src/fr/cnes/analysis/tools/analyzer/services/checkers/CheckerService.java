package fr.cnes.analysis.tools.analyzer.services.checkers;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

public class CheckerService {

    public static final String CHECKER_EP_ID = "fr.cnes.analysis.tools.checks";
    public static final String CHECKER_EP_NAME = "checks";
    public static final String CHECKER_EP_ELEMENT_CHECK = "check";
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_NAME = "name";
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_ID = "id";
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID = "languageId";
    public static final String CHECKER_EP_ELEMENT_CHECK_ATT_CLASS = "class";

    public List<CheckerContainer> getCheckers() throws NullContributionException, CoreException {
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
            LanguageService languageService = new LanguageService();
            LanguageContainer language = languageService.getLanguage(
                    checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
            // 1.4 Import the AbstractChecker class thanks
            AbstractChecker checkerClass = (AbstractChecker) checkerElement
                    .createExecutableExtension(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
            // 1.5 Create the Checker
            CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                    checkerClass, checkerElement);

            // 1.6 Add the checkers to all checkers.
            checkers.add(checker);
        }
        return checkers;
    }

    public List<CheckerContainer> getCheckers(String languageId)
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
                LanguageService languageService = new LanguageService();
                LanguageContainer language = languageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Create the Checker
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement);

                // 1.6 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
        return checkers;
    }

    public List<CheckerContainer> getCheckers(List<String> checkersIds)
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
                LanguageService languageService = new LanguageService();
                LanguageContainer language = languageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Create the Checker
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement);

                // 1.6 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }
        return checkers;
    }

    public List<CheckerContainer> getCheckers(List<String> languagesIds, List<String> excludedIds)
            throws NullContributionException, CoreException {
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
                LanguageService languageService = new LanguageService();
                LanguageContainer language = languageService.getLanguage(
                        checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                // 1.4 Import the AbstractChecker class thanks
                AbstractChecker checkerClass = (AbstractChecker) checkerElement
                        .createExecutableExtension(
                                CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                // 1.5 Create the Checker
                CheckerContainer checker = new CheckerContainer(checkerId, checkerName, language,
                        checkerClass, checkerElement);

                // 1.6 Add the checkers to all checkers.
                checkers.add(checker);
            }
        }

        return checkers;

    }

    public boolean isCheckerIdContributor(String id) {
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerElement : checkerContributors) {
            if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID).equals(id)) {
                return true;
            }
        }
        return false;
    }

    public CheckerContainer getChecker(String id) throws NullContributionException, CoreException {
        IConfigurationElement[] checkerContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(CHECKER_EP_ID);
        for (IConfigurationElement checkerContributor : checkerContributors) {
            for (IConfigurationElement checkerElement : checkerContributor
                    .getChildren(CheckerService.CHECKER_EP_ELEMENT_CHECK)) {
                if (checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_ID).equals(id)) {
                    // 1.1. Get the checker name
                    String checkerName = checkerElement
                            .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_NAME);
                    // 1.2. Get the checker id;
                    String checkerId = checkerElement
                            .getAttribute(CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_ID);
                    // 1.3 Get the Language thanks to the checker id;
                    LanguageService languageService = new LanguageService();
                    LanguageContainer language = languageService.getLanguage(
                            checkerElement.getAttribute(CHECKER_EP_ELEMENT_CHECK_ATT_LANGUAGEID));
                    // 1.4 Import the AbstractChecker class thanks
                    AbstractChecker checkerClass = (AbstractChecker) checkerElement
                            .createExecutableExtension(
                                    CheckerService.CHECKER_EP_ELEMENT_CHECK_ATT_CLASS);
                    // 1.5 Create the Checker
                    return new CheckerContainer(checkerId, checkerName, language, checkerClass,
                            checkerElement);

                }
            }
        }
        throw new NullContributionException(
                "Impossible to find " + id + " checker id in contributors.");
    }

}
