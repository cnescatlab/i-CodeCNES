/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.checkers;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.IChecker;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.analyzer.services.languages.ILanguage;

import java.util.List;

/**
 * Container for checker defined in contribution of the
 * {@link CheckerService#CHECKER_EP_ID} extension point to be used by
 * {@link CheckerService} to access data.
 * 
 * @since 3.0
 */
public class CheckerContainer {

    /** Class name **/
    private static final String CLASS = CheckerContainer.class.getName();

    /** Checker identifier */
    private String id;
    /** Checker name */
    private String name;
    /** Checker's language */
    private ILanguage language;
    /** Checker's analysis class */
    private IChecker checker;
    /** Whether or not the checker is a metric */
    private boolean isMetric;

    /**
     * @param pId
     *            Checker identifier.
     * @param pName
     *            Checker name.
     * @param pLanguage
     *            Checker's language.
     * @param pChecker
     *            Checker analysis class.
     */
    public CheckerContainer(final String pId, final String pName, final ILanguage pLanguage,
                    final IChecker pChecker) {
        final String method = "";
        ICodeLogger.entering(CLASS, method);

        this.id = pId;
        this.name = pName;
        this.language = pLanguage;
        this.checker = pChecker;
        this.isMetric = false;
    }

    /**
     * @param pId
     *            Checker identifier.
     * @param pName
     *            Checker name.
     * @param pLanguage
 *            Checker's language.
     * @param pChecker
*            Checker analysis class.
     * @param pIsMetric
     */
    public CheckerContainer(final String pId, final String pName, final ILanguage pLanguage,
                            final IChecker pChecker, boolean pIsMetric) {
        final String method = "CheckerContainer";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pId, pName, pLanguage, pChecker, pIsMetric
        });
        this.id = pId;
        this.name = pName;
        this.language = pLanguage;
        this.checker = pChecker;
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pFormat
     *            to test
     * @return whether or not the format is handled by this checker.
     */
    public boolean canVerifyFormat(final String pFormat) {
        final String method = "canVerifyFormat";
        ICodeLogger.entering(CLASS, method, pFormat);
        final boolean verify = this.language.getFileExtension().contains(pFormat);
        ICodeLogger.exiting(CLASS, method, verify);
        return verify;
    }

    /**
     * @return format that can be handled by this checker's language.
     */
    public List<String> getVerifiableFormat() {
        final String method = "getVerifiableFormat";
        ICodeLogger.entering(CLASS, method);
        final List<String> fileFormats = this.language.getFileExtension();
        ICodeLogger.exiting(CLASS, method, fileFormats);
        return fileFormats;
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
     * @return the language
     */
    public final ILanguage getLanguage() {
        final String method = "getLanguage";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, language);
        return language;
    }

    /**
     * @param pLanguage
     *            the language to set
     */
    protected final void setLanguage(final ILanguage pLanguage) {
        final String method = "setLanguage";
        ICodeLogger.entering(CLASS, method, pLanguage);
        this.language = pLanguage;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the checker A clone class of the checker.
     */
    public final AbstractChecker getChecker() {
        final String method = "getChecker";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, null);
        return null;
    }

    /**
     * @param pChecker
     *            the checker to set
     */
    protected final void setChecker(final AbstractChecker pChecker) {
        final String method = "setChecker";
        ICodeLogger.entering(CLASS, method, pChecker);
        this.checker = pChecker;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the isMetric
     */
    public final boolean isMetric() {
        final String method = "isMetric";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, isMetric);
        return isMetric;
    }

    /**
     * @param pIsMetric
     *            the isMetric to set
     */
    public final void setMetric(final boolean pIsMetric) {
        final String method = "setMetric";
        ICodeLogger.entering(CLASS, method, pIsMetric);
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);
    }
}
