package fr.cnes.analysis.tools.analyzer.services.languages;

import java.util.List;

/**
 * Container for a language plugged by a contributor for {@link LanguageService}
 * class.
 *
 */
public class LanguageContainer {
    private String name;
    private String id;
    private List<String> fileExtension;

    public LanguageContainer(String pId, String pName, List<String> pFileExtension) {
        this.id = pId;
        this.name = pName;
        this.fileExtension = pFileExtension;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    protected final void setName(String name) {
        this.name = name;
    }

    /**
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    protected final void setId(String id) {
        this.id = id;
    }

    /**
     * @return the fileExtension
     */
    public final List<String> getFileExtension() {
        return fileExtension;
    }

    /**
     * @param fileExtension
     *            the fileExtension to set
     */
    protected final void setFileExtension(List<String> fileExtension) {
        this.fileExtension = fileExtension;
    }

}
