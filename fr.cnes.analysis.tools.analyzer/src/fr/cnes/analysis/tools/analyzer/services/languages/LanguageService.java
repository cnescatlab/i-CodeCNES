package fr.cnes.analysis.tools.analyzer.services.languages;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

public class LanguageService {

    public static final String LANGUAGE_EP_ID = "fr.cnes.analysis.tools.languages";
    public static final String LANGUAGE_EP_NAME = "languages";

    public static final String LANGUAGE_EP_EL_LANGUAGE = "language";
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_ID = "id";
    public static final String LANGUAGE_EP_EL_LANGUAGE_ATT_NAME = "name";
    public static final String LANGUAGE_EP_EL_FILEEXTENSION = "fileExtension";
    public static final String LANGUAGE_EP_EL_FILEEXTENSION_ATT_NAME = "name";

    /**
     * @return
     */
    public List<LanguageContainer> getLanguages() {
        List<LanguageContainer> languages = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement languageContributor : languagesContributors) {
            for (IConfigurationElement language : languageContributor
                    .getChildren(LanguageService.LANGUAGE_EP_EL_LANGUAGE)) {
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
     * @param languageId
     * @return
     */
    public boolean isLanguageIdContributor(String languageId) {
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement languageContributor : languagesContributors) {
            for (IConfigurationElement language : languageContributor
                    .getChildren(LanguageService.LANGUAGE_EP_EL_LANGUAGE)) {
                if (language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID)
                        .equals(languageId)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param languageId
     * @return
     * @throws NullContributionException
     */
    public LanguageContainer getLanguage(String languageId) throws NullContributionException {
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement languageContributor : languagesContributors) {
            for (IConfigurationElement language : languageContributor
                    .getChildren(LanguageService.LANGUAGE_EP_EL_LANGUAGE)) {
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
                    return new LanguageContainer(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID,
                            LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_NAME, fileExtensions);
                }
            }
        }
        throw new NullContributionException(
                "Impossible to find " + languageId + " in analyzer contributors.");
    }

    /**
     * @param languagesIds
     * @return
     */
    public List<LanguageContainer> getLanguages(List<String> languagesIds) {
        List<LanguageContainer> languages = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement languageContributor : languagesContributors) {
            for (IConfigurationElement language : languageContributor
                    .getChildren(LanguageService.LANGUAGE_EP_EL_LANGUAGE)) {
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
        }

        return languages;
    }

    /**
     * @return
     */
    public List<String> getLanguagesIds() {
        List<String> languagesIds = new ArrayList<>();
        IConfigurationElement[] languagesContributors = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(LanguageService.LANGUAGE_EP_ID);
        for (IConfigurationElement languageContributor : languagesContributors) {
            for (IConfigurationElement language : languageContributor
                    .getChildren(LanguageService.LANGUAGE_EP_EL_LANGUAGE)) {
                languagesIds
                        .add(language.getAttribute(LanguageService.LANGUAGE_EP_EL_LANGUAGE_ATT_ID));
            }
        }
        return languagesIds;
    }

}
