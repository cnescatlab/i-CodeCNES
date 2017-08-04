/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.languages;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;

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
public final class LanguageService {

    /** Language extension point identifier */
    public static final String LANGUAGE_EP_ID = "fr.cnes.analysis.tools.analyzer.languages";
    /** Language extension point name */
    public static final String LANGUAGE_EP_NAME = "languages";
    /** Language extension point's language element */
    public static final String LANGUAGE_EP_EL_LANGUAGE = "language";
    /** Language extension point's language element's identifier attribute */
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_ID = "id";
    /** Language extension point's language element's name attribute */
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_NAME = "name";
    /** Language extension point's file extensions element */
    public static final String LANGUAGE_EP_EL_FILEEXTENSION = "fileExtension";
    /** Language extension point's file extension element's attribute name */
    public static final String LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME = "name";

    /**
     * Utils class default constructor removal.
     */
    private LanguageService() {
        // Nothing here.
    }

    /**
     * @return languages contributing to {@link #LANGUAGE_EP_ID}.
     */
    public static List<LanguageContainer> getLanguages() {
        final List<LanguageContainer> languages = new ArrayList<>();
        final IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            final List<String> fileExtensions = new ArrayList<>();
            for (IConfigurationElement fileExtensionElement : language
                            .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                final String fileExtension = fileExtensionElement
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
        boolean isLanguageIdContributor = false;
        final IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID)
                            .equals(languageId)) {
                isLanguageIdContributor = true;
            }
        }
        return isLanguageIdContributor;
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
        final IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID)
                            .equals(languageId)) {
                final ArrayList<String> fileExtensions = new ArrayList<>();
                for (IConfigurationElement fileExtensionElement : language
                                .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                    final String fileExtension = fileExtensionElement
                                    .getAttribute(LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME);
                    if (fileExtension.contains(".")) {
                        fileExtensions.add(fileExtension.replace(".", ""));
                    } else {
                        fileExtensions.add(fileExtension);
                    }
                }
                return new LanguageContainer(
                                language.getAttribute(
                                                LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID),
                                language.getAttribute(
                                                LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_NAME),
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
        final List<LanguageContainer> languages = new ArrayList<>();
        final IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            if (languagesIds.contains(language
                            .getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID))) {
                final List<String> fileExtensions = new ArrayList<>();
                for (IConfigurationElement fileExtensionElement : language
                                .getChildren(LanguageService.LANGUAGE_EP_EL_FILEEXTENSION)) {
                    final String fileExtension = fileExtensionElement
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
        final List<String> languagesIds = new ArrayList<>();
        final IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement language : languagesContributors) {
            languagesIds.add(language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID));
        }
        return languagesIds;
    }

}
