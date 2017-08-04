/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.languages;

import java.util.List;

/**
 * Container for a language plugged by a contributor for {@link LanguageService}
 * class.
 *
 */
public class LanguageContainer {

    /**
     * Language name;
     */
    private String name;
    /**
     * language identifier;
     */
    private String id;
    /**
     * Files extensions for this languages.
     */
    private List<String> fileExtension;

    /**
     * @param pId
     *            Identifier of the language.
     * @param pName
     *            Name of the language.
     * @param pFileExtension
     *            List of extensions that can be handled by the language in
     *            contribution.
     */
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
     * @param pName
     *            the name to set
     */
    protected final void setName(String pName) {
        this.name = pName;
    }

    /**
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param pId
     *            the id to set
     */
    protected final void setId(String pId) {
        this.id = pId;
    }

    /**
     * @return the fileExtension
     */
    public final List<String> getFileExtension() {
        return fileExtension;
    }

    /**
     * @param pFileExtension
     *            the fileExtension to set
     */
    protected final void setFileExtension(List<String> pFileExtension) {
        this.fileExtension = pFileExtension;
    }

}
