/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.languages;

import java.util.List;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * Container for a language plugged by a contributor for {@link LanguageService}
 * class.
 *
 */
public class LanguageContainer {

    /** Class name */
    private static final String CLASS = LanguageContainer.class.getName();

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
    public LanguageContainer(final String pId, final String pName,
                    final List<String> pFileExtension) {
        final String method = "LanguageContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, pName, pFileExtension
        });
        this.id = pId;
        this.name = pName;
        this.fileExtension = pFileExtension;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the name
     */
    public final String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, name);
        return name;
    }

    /**
     * @param pName
     *            the name to set
     */
    protected final void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the id
     */
    public final String getId() {
        final String method = "getId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, id);
        return id;
    }

    /**
     * @param pId
     *            the id to set
     */
    protected final void setId(final String pId) {
        final String method = "setId";
        ICodeLogger.entering(CLASS, method, pId);
        this.id = pId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the fileExtension
     */
    public final List<String> getFileExtension() {
        final String method = "getFileExtension";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, fileExtension);
        return fileExtension;
    }

    /**
     * @param pFileExtension
     *            the fileExtension to set
     */
    protected final void setFileExtension(final List<String> pFileExtension) {
        final String method = "setFileExtension";
        ICodeLogger.entering(CLASS, method, pFileExtension);
        this.fileExtension = pFileExtension;
        ICodeLogger.exiting(CLASS, method);
    }

}
