/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.services.checkers;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.exception.NullContributionException;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.languages.ILanguage;
import fr.cnes.icode.services.languages.LanguageService;

import javax.xml.bind.annotation.XmlAttribute;
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
    private Class<?> checker;
    /** Whether or not the checker is a metric */
    private boolean isMetric;

    /**
     * Default constructor.
     */
    public CheckerContainer() {
        final String method = "CheckerContainer";
        ICodeLogger.entering(CLASS, method);
        this.id = "";
        this.name = "";
        this.language = null;
        this.checker = null;
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
     */
    public CheckerContainer(final String pId, final String pName, final ILanguage pLanguage,
                    final Class<?> pChecker) {
        final String method = "CheckerContainer";
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
     *            True if the checker is a metric.
     */
    public CheckerContainer(final String pId, final String pName, final ILanguage pLanguage,
                            final Class<?> pChecker, boolean pIsMetric) {
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
     * Getter for the {@link #id} of the checker.
     *
     * @return the corresponding string.
     */
    public final String getId() {
        final String method = "getId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, id);
        return id;
    }

    /**
     * Setter for the {@link #id} of the checker.
     *
     * @param pId New ID.
     */
    @XmlAttribute(name="id")
    protected final void setId(final String pId) {
        final String method = "setId";
        ICodeLogger.entering(CLASS, method, pId);
        this.id = pId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the {@link #name} of the checker.
     *
     * @return the corresponding string.
     */
    public final String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, name);
        return name;
    }

    /**
     * Setter for the {@link #language} of the checker.
     *
     * @param pName New name.
     */
    @XmlAttribute(name="name")
    protected final void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the {@link #language} of the checker.
     *
     * @return the corresponding string.
     */
    public final ILanguage getLanguage() {
        final String method = "getLanguage";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, language);
        return language;
    }

    /**
     * Setter for the {@link #language} of the checker.
     *
     * @param pLanguageId New language ID.
     */
    @XmlAttribute(name="languageId")
    protected final void setLanguageId(final String pLanguageId) {
        final String method = "setLanguageId";
        ICodeLogger.entering(CLASS, method, pLanguageId);
        try {
            this.language = LanguageService.getLanguage(pLanguageId);
        } catch (final NullContributionException e) {
            ICodeLogger.error(CLASS, method, e);
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the {@link #checker} instantiation.
     *
     * @return instantiation as AbstractChecker object.
     */
    public final AbstractChecker getChecker() {
        final String method = "getChecker";
        ICodeLogger.entering(CLASS, method);
        AbstractChecker result = null;
        try {
            if(checker != null) {
                result = (AbstractChecker) checker.newInstance();
                result.setId(this.getId());
                result.setName(this.getName());
                result.setLanguageId(this.getLanguage().getId());
            }
        } catch (final InstantiationException|IllegalAccessException e) {
            ICodeLogger.error(CLASS, method, e);
        }
        ICodeLogger.exiting(CLASS, method, null);
        return result;
    }

    /**
     * Setter for the {@link #checker} of the checker.
     *
     * @param pChecker New checker class.
     */
    protected final void setChecker(final Class<?> pChecker) {
        final String method = "setChecker";
        ICodeLogger.entering(CLASS, method, pChecker);
        this.checker = pChecker;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the {@link #checker} of the checker.
     *
     * @param pClass New checker class.
     */
    @XmlAttribute(name="class")
    protected final void setClass(final String pClass) {
        final String method = "setClass";
        ICodeLogger.entering(CLASS, method, pClass);
        try {
            this.checker = Class.forName(pClass);
        } catch (final ClassNotFoundException e) {
            ICodeLogger.error(CLASS, method, e);
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the {@link #isMetric} of the checker.
     *
     * @return the corresponding string.
     */
    public final boolean isMetric() {
        final String method = "isMetric";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, isMetric);
        return isMetric;
    }

    /**
     * Setter for the {@link #isMetric} of the checker.
     *
     * @param pIsMetric New checker metric status.
     */
    @XmlAttribute(name="isMetric")
    public final void setMetric(final boolean pIsMetric) {
        final String method = "setMetric";
        ICodeLogger.entering(CLASS, method, pIsMetric);
        this.isMetric = pIsMetric;
        ICodeLogger.exiting(CLASS, method);
    }
}
