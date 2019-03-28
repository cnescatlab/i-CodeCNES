/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.icode.services.checkers;

import com.google.common.collect.Lists;
import fr.cnes.icode.datas.CheckersDefinition;
import fr.cnes.icode.exception.NullContributionException;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.reflexion.ClassFinder;
import fr.cnes.icode.services.languages.LanguageService;

import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This service can be used to reach Checker available in
 * {@link #CHECKER_EP_ID}. Some function of this service returns Checker in
 * {@link CheckerContainer}.
 *
 * @since 3.0
 */
public final class CheckerService {

    /** Extension Point ID . */
    public static final String CHECKER_EP_ID = "fr.cnes.icode.checks";

    /** Class name **/
    private static final String CLASS = CheckerService.class.getName();

    /** CheckerContainers cache. **/
    private static List<CheckerContainer> checkerContainers = null;

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
        final List<CheckerContainer> checkerContainers;

        if(Objects.isNull(CheckerService.checkerContainers)) {
            checkerContainers = Lists.newArrayList();
            try {
                Set<Class<?>> classes = ClassFinder.find(CheckersDefinition.class);
                for(final Class current : classes) {
                    final CheckersDefinition definition = (CheckersDefinition) current.newInstance();
                    checkerContainers.addAll(definition.list());
                }
            } catch (final Exception e) {
                ICodeLogger.error(CLASS, method, e);
            }
            CheckerService.checkerContainers = checkerContainers;
        } else {
            checkerContainers = CheckerService.checkerContainers;
        }

        ICodeLogger.exiting(CLASS, method, checkerContainers);
        return checkerContainers;
    }

    /**
     * @param languageId
     *            to retrieve checker's from.
     * @return a list of {@link CheckerContainer} contributing to
     *         {@link #CHECKER_EP_ID} and referencing <code>languageId</code>
     *         parameter.
     */
    public static List<CheckerContainer> getCheckers(final String languageId) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, languageId);

        final List<CheckerContainer> checkers = getCheckers();

        final List<CheckerContainer> results = checkers.stream()
                .filter(x->languageId.equals(x.getLanguage().getId())).collect(Collectors.toList());

        ICodeLogger.exiting(CLASS, method, results);
        return results;
    }

    /**
     * @param languageId
     *            the language identifier to retrieve checkers from.
     * @return A list of every checker's ids referencing languageId.
     */
    public static List<String> getCheckersIds(final String languageId) {
        final String method = "getCheckersIds";
        ICodeLogger.entering(CLASS, method, languageId);
        final List<String> checkers = getCheckers(languageId).stream()
                .map(CheckerContainer::getId).collect(Collectors.toList());
        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param checkersIds
     *            list of checkers identifiers to retrieve
     *            {@link CheckerContainer} class from.
     * @return A list of {@link CheckerContainer} for each checker identifier in
     *         <code>checkersIds</code>.
     */
    public static List<CheckerContainer> getCheckers(final List<String> checkersIds) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, checkersIds);

        final List<CheckerContainer> checkers = getCheckers().stream()
                .filter(x->checkersIds.contains(x.getId())).collect(Collectors.toList());

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
     */
    public static List<CheckerContainer> getCheckers(final List<String> languagesIds,
                                                     final List<String> excludedIds) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, new Object[] {
                languagesIds, excludedIds
        });

        final List<CheckerContainer> checkers = getCheckers().stream()
                .filter(x->((!excludedIds.contains(x.getId())) && languagesIds.contains(x.getLanguage().getId())))
                .collect(Collectors.toList());

        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param languagesId
     *            language identifier to retrieve checkers from.
     * @param excludedIds
     *            checkers identifier to not include in the returned checkers.
     * @return a list of {@link CheckerContainer} containing all checkers
     *         referencing a language identifier in <code>languagesIds</code>
     *         except the ones contained in <code>excludedIds</code>.
     */
    public static List<CheckerContainer> getCheckers(final String languagesId,
                                                     final List<String> excludedIds) {
        final String method = "getCheckers";
        ICodeLogger.entering(CLASS, method, new Object[] {
                languagesId, excludedIds
        });

        final List<CheckerContainer> checkers = getCheckers().stream()
                .filter(x->((!excludedIds.contains(x.getId())) && languagesId.equals(x.getLanguage().getId())))
                .collect(Collectors.toList());

        ICodeLogger.exiting(CLASS, method, checkers);
        return checkers;
    }

    /**
     * @param checkerId
     *            checker identifier to verify contribution
     * @return whether or not the checker identifier in <code>checkerId</code>
     *         parameter is contributing to {@link #CHECKER_EP_ID}.
     */
    public static boolean isCheckerIdContributor(final String checkerId) {
        final String method = "isCheckerIdContributor";
        ICodeLogger.entering(CLASS, method, checkerId);
        boolean isCheckerIdContributor = false;
        final Iterator<CheckerContainer> checkers = getCheckers().iterator();
        while(checkers.hasNext() && !isCheckerIdContributor) {
            isCheckerIdContributor = checkers.next().getId().equals(checkerId);
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
     */
    public static CheckerContainer getChecker(final String checkerId) throws NullContributionException {
        final String method = "getChecker";
        ICodeLogger.entering(CLASS, checkerId);

        CheckerContainer checker = null;
        final Iterator<CheckerContainer> checkers = getCheckers().iterator();

        while(checkers.hasNext() && Objects.isNull(checker)) {
            CheckerContainer current = checkers.next();
            if(current.getId().equals(checkerId)) {
                checker = current;
            }
        }

        if(Objects.isNull(checker)) {
            final NullContributionException exception = new NullContributionException(
                    "Impossible to find " + checkerId + " in analyzer contributors.");
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }

        ICodeLogger.exiting(CLASS, method, checker);
        return checker;
    }

}
