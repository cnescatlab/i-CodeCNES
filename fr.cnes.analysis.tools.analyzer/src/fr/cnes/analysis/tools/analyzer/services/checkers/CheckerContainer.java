/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.services.checkers;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;

/**
 * Container for checker defined in contribution of the
 * {@link CheckerService#CHECKER_EP_ID} extension point to be used by
 * {@link CheckerService} to access data.
 * 
 * @since 3.0
 */
public class CheckerContainer {

    /** Checker identifier */
    private String id;
    /** Checker name */
    private String name;
    /** Checker's language */
    private LanguageContainer language;
    /** Checker's analysis class */
    private AbstractChecker checker;
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
     * @param pCheckerContribution
     *            Checker's contribution.
     */
    public CheckerContainer(String pId, String pName, LanguageContainer pLanguage,
                    AbstractChecker pChecker, IConfigurationElement pCheckerContribution) {
        this.id = pId;
        this.name = pName;
        this.language = pLanguage;
        this.checker = pChecker;
        this.checker.setContribution(pCheckerContribution);
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
     * @param pCheckerContribution
     *            Checker's contribution.
     * @param pIsMetric
     *            whether or not the checker returns a value.
     */
    public CheckerContainer(String pId, String pName, LanguageContainer pLanguage,
                    AbstractChecker pChecker, IConfigurationElement pCheckerContribution,
                    boolean pIsMetric) {
        this.id = pId;
        this.name = pName;
        this.language = pLanguage;
        this.checker = pChecker;
        this.checker.setContribution(pCheckerContribution);
        this.isMetric = pIsMetric;
    }

    /**
     * @param pFormat
     *            to test
     * @return whether or not the format is handled by this checker.
     */
    public boolean canVerifyFormat(String pFormat) {
        return this.language.getFileExtension().contains(pFormat);
    }

    /**
     * @return format that can be handled by this checker's language.
     */
    public List<String> getVerifiableFormat() {
        return this.language.getFileExtension();
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
     * @return the language
     */
    public final LanguageContainer getLanguage() {
        return language;
    }

    /**
     * @param pLanguage
     *            the language to set
     */
    protected final void setLanguage(LanguageContainer pLanguage) {
        this.language = pLanguage;
    }

    /**
     * @return the checker
     */
    public final AbstractChecker getChecker() {
        return checker;
    }

    /**
     * @param pChecker
     *            the checker to set
     */
    protected final void setChecker(AbstractChecker pChecker) {
        this.checker = pChecker;
    }

    /**
     * @return the isMetric
     */
    public final boolean isMetric() {
        return isMetric;
    }

    /**
     * @param pIsMetric
     *            the isMetric to set
     */
    public final void setMetric(boolean pIsMetric) {
        this.isMetric = pIsMetric;
    }
}
