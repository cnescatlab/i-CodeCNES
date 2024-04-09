/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.services.languages;

import fr.cnes.icode.exception.NullContributionException;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.reflexion.ClassFinder;

import java.util.*;
import java.util.stream.Collectors;

/**
 * This service must be used to access data from the extension point
 * {@link #LANGUAGE_EP_ID}.
 * 
 * <p>
 * <b>Note : </b>Part of the functions of this class uses
 * {@link ILanguage} class to returns data.
 * </p>
 * 
 * @since 3.0
 */
public final class LanguageService {

    /** Language extension point identifier */
    public static final String LANGUAGE_EP_ID = "fr.cnes.icode.languages";

    /** Class name **/
    private static final String CLASS = LanguageService.class.getName();

    /** Languages cache. **/
    private static List<ILanguage> languages = null;

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
        final List<ILanguage> languages;

        if(Objects.isNull(LanguageService.languages) || LanguageService.languages.isEmpty()) {
            languages = new ArrayList<>();
            try {
                Set<Class<?>> classes = ClassFinder.find(ILanguage.class);
                for (final Class current : classes) {
                    final ILanguage language = (ILanguage) current.newInstance();
                    languages.add(language);
                }
            } catch (final Exception e) {
                ICodeLogger.error(CLASS, method, e);
            }
            LanguageService.languages = languages;
        } else {
            languages = LanguageService.languages;
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
        final Iterator<ILanguage> languages = getLanguages().iterator();

        while(languages.hasNext() && !isLanguageIdContributor) {
            isLanguageIdContributor = languages.next().getId().equals(languageId);
        }

        ICodeLogger.exiting(CLASS, method);
        return isLanguageIdContributor;
    }

    /**
     * @param languageId
     *            Identifier of the language.
     * @return A {@link ILanguage} fulfilled of data retrieved from the
     *         contribution.
     * @throws NullContributionException
     *             When <code>languageId</code> is not contributing to
     *             {@link #LANGUAGE_EP_ID}.
     */
    public static ILanguage getLanguage(final String languageId)
                    throws NullContributionException {
        final String method = "getLanguage";
        ICodeLogger.entering(CLASS, method, languageId);

        ILanguage language = null;
        final Iterator<ILanguage> languages = getLanguages().iterator();

        while(languages.hasNext() && Objects.isNull(language)) {
            ILanguage current = languages.next();
            if(current.getId().equals(languageId)) {
                language = current;
            }
        }

        if(Objects.isNull(language)) {
            final NullContributionException exception = new NullContributionException(
                    "Impossible to find " + languageId + " among " + getLanguages().size() + " analyzer contributors.");
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }

        return language;
    }

    /**
     * @param languagesIds
     *            Languages identifiers
     * @return List of {@link ILanguage} fulfilled with data retrieved
     *         from <code>languagesIds</code> contribution.
     */
    public static List<ILanguage> getLanguages(final List<String> languagesIds) {
        final String method = "getLanguages";
        ICodeLogger.entering(CLASS, method, languagesIds);

        final List<ILanguage> results = getLanguages().stream()
                .filter(x->languagesIds.contains(x.getId())).collect(Collectors.toList());

        ICodeLogger.exiting(CLASS, method, results);
        return results;
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

    /**
     * @return The id of the language corresponding to the given file extension.
     */
    public static String getLanguageId(final String pExtension) {
        final String method = "getLanguageId";
        ICodeLogger.entering(CLASS, method);
        final Optional<ILanguage> language = getLanguages().stream().filter(x -> x.getFileExtension().contains(pExtension)).findFirst();
        String languageId = "";
        if(language.isPresent()) {
            languageId = language.get().getId();
        }
        ICodeLogger.exiting(CLASS, method, languageId);
        return languageId;
    }

}
