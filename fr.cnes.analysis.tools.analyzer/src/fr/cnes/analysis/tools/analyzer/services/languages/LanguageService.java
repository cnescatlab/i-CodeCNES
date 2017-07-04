package fr.cnes.analysis.tools.analyzer.services.languages;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

/**
 * This service must be used to access data from the extension point
 * {@link #LANGUAGE_EP_ID}.
 * 
 * <p>
 * <b>Note : </b>Part of the functions of this class uses
 * {@link LanguageContainer} class to returns data.
 * </p>
 * 
 * @since 3.0
 */
public class LanguageService {

    public static final String LANGUAGE_EP_ID = "fr.cnes.analysis.tools.analyzer.languages";
    public static final String LANGUAGE_EP_NAME = "languages";
    public static final String LANGUAGE_EP_EL_LANGUAGE = "language";
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_ID = "id";
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_NAME = "name";
    public static final String LANGUAGE_EP_EL_FILEEXTENSION = "fileExtension";
    public static final String LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME = "name";

    /**
     * @return languages contributing to {@link #LANGUAGE_EP_ID}.
     */
    public static List<LanguageContainer> getLanguages() {
        List<LanguageContainer> languages = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            List<String> fileExtensions = new ArrayList<>();
            for (IConfigurationElement fileExtensionElement : language
                    .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                String fileExtension = fileExtensionElement
                        .getAttribute(LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME);
                if (fileExtension.contains(".")) {
                    fileExtensions.add(fileExtension.replace(".", ""));
                } else {
                    fileExtensions.add(fileExtension);
                }
            }
            languages.add(new LanguageContainer(
                    language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID),
                    language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_NAME),
                    fileExtensions));
        }

        return languages;

    }

    /**
     * @param languageId
     *            language identifier
     * @return whether or not this language identifier contribute to
     *         {@link #LANGUAGE_EP_ID}
     */
    public static boolean isLanguageIdContributor(String languageId) {
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID)
                    .equals(languageId)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param languageId
     *            Identifier of the language.
     * @return A {@link LanguageContainer} fulfilled of data retrieved from the
     *         contribution.
     * @throws NullContributionException
     *             When <code>languageId</code> is not contributing to
     *             {@link #LANGUAGE_EP_ID}.
     */
    public static LanguageContainer getLanguage(String languageId)
            throws NullContributionException {
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID)
                    .equals(languageId)) {
                ArrayList<String> fileExtensions = new ArrayList<>();
                for (IConfigurationElement fileExtensionElement : language
                        .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                    String fileExtension = fileExtensionElement
                            .getAttribute(LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME);
                    if (fileExtension.contains(".")) {
                        fileExtensions.add(fileExtension.replace(".", ""));
                    } else {
                        fileExtensions.add(fileExtension);
                    }
                }
                return new LanguageContainer(
                        language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID),
                        language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_NAME),
                        fileExtensions);
            }
        }
        throw new NullContributionException(
                "Impossible to find " + languageId + " in analyzer contributors.");

    }

    /**
     * @param languagesIds
     *            Languages identifiers
     * @return List of {@link LanguageContainer} fulfilled with data retrieved
     *         from <code>languagesIds</code> contribution.
     */
    public static List<LanguageContainer> getLanguages(List<String> languagesIds) {
        List<LanguageContainer> languages = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (languagesIds.contains(
                    language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID))) {
                List<String> fileExtensions = new ArrayList<>();
                for (IConfigurationElement fileExtensionElement : language
                        .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                    String fileExtension = fileExtensionElement
                            .getAttribute(LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME);
                    if (fileExtension.contains(".")) {
                        fileExtensions.add(fileExtension.replace(".", ""));
                    } else {
                        fileExtensions.add(fileExtension);
                    }
                }
                languages.add(new LanguageContainer(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID,
                        LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_NAME, fileExtensions));
            }
        }

        return languages;
    }

    /**
     * @return Every language identifiers contributing to
     *         {@link #LANGUAGE_EP_ID}.
     */
    public static List<String> getLanguagesIds() {
        List<String> languagesIds = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            languagesIds.add(language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID));
        }
        return languagesIds;
    }

}
