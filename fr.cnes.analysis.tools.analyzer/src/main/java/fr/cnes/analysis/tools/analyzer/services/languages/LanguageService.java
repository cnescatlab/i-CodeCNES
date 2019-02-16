/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.languages;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.analyzer.reflexion.ClassFinder;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This service must be used to access data from the extension point
 * {@link #LANGUAGE_EP_ID}.
 * 
 * <p>
 * <b>Note : </b>Part of the functions of this class uses
 * {@link Language} class to returns data.
 * </p>
 * 
 * @since 3.0
 */
public final class LanguageService {

    /** Language extension point identifier */
    public static final String LANGUAGE_EP_ID = "fr.cnes.analysis.tools.analyzer.languages";

    /** Class name **/
    private static final String CLASS = LanguageService.class.getName();

    /**
     * Utils class default constructor removal.
     */
    private LanguageService() {
        // Nothing here.
    }

    /**
     * @return languages contributing to {@link #LANGUAGE_EP_ID}.
     */
    public static List<ILanguage> getLanguages() {
        final String method = "getLanguages";
        ICodeLogger.entering(CLASS, method);
        final List<ILanguage> languages = new ArrayList<>();

        try {
            Set<Class<?>> classes = ClassFinder.find(ILanguage.class);
            for(final Class current : classes) {
                final ILanguage language = (ILanguage) current.newInstance();
                languages.add(language);
            }
        } catch (final Exception e) {
            ICodeLogger.error(CLASS, method, e);
        }

        ICodeLogger.exiting(CLASS, method, languages);
        return languages;

    }

    /**
     * @param languageId
     *            language identifier
     * @return whether or not this language identifier contribute to
     *         {@link #LANGUAGE_EP_ID}
     */
    public static boolean isLanguageIdContributor(final String languageId) {
        final String method = "isLanguageIdContributor";
        ICodeLogger.entering(CLASS, method, languageId);
        boolean isLanguageIdContributor = false;
        ICodeLogger.exiting(CLASS, method);
        return isLanguageIdContributor;
    }

    /**
     * @param languageId
     *            Identifier of the language.
     * @return A {@link Language} fulfilled of data retrieved from the
     *         contribution.
     * @throws NullContributionException
     *             When <code>languageId</code> is not contributing to
     *             {@link #LANGUAGE_EP_ID}.
     */
    public static ILanguage getLanguage(final String languageId)
                    throws NullContributionException {
        final String method = "getLanguage";
        ICodeLogger.entering(CLASS, method, languageId);
        final NullContributionException exception = new NullContributionException(
                        "Impossible to find " + languageId + " in analyzer contributors.");
        ICodeLogger.throwing(CLASS, method, exception);
        throw exception;
    }

    /**
     * @param languagesIds
     *            Languages identifiers
     * @return List of {@link Language} fulfilled with data retrieved
     *         from <code>languagesIds</code> contribution.
     */
    public static List<ILanguage> getLanguages(final List<String> languagesIds) {
        final String method = "getLanguages";
        ICodeLogger.entering(CLASS, method, languagesIds);
        final List<ILanguage> languages = new ArrayList<>();

        ICodeLogger.exiting(CLASS, method, languages);
        return languages;
    }

    /**
     * @return Every language identifiers contributing to
     *         {@link #LANGUAGE_EP_ID}.
     */
    public static List<String> getLanguagesIds() {
        final String method = "getLanguagesIds";
        ICodeLogger.entering(CLASS, method);
        final List<String> languagesIds = getLanguages().stream().map(ILanguage::getId).collect(Collectors.toList());
        ICodeLogger.exiting(CLASS, method, languagesIds);
        return languagesIds;
    }

}
