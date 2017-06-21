package fr.cnes.analysis.tools.analyzer.services.checkers;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageContainer;
import java.util.List;
import org.eclipse.core.runtime.IConfigurationElement;

public class CheckerContainer {

    private String id;
    private String name;
    private LanguageContainer language;
    private AbstractChecker checker;

    public CheckerContainer(String pId, String pName, LanguageContainer pLanguage, AbstractChecker pChecker,
            IConfigurationElement pCheckerContribution) {
        this.id = pId;
        this.name = pName;
        this.language = pLanguage;
        this.checker = pChecker;
        this.checker.setContribution(pCheckerContribution);
    }

    public boolean canVerifyFormat(String format) {
        return this.language.getFileExtension().contains(format);
    }

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
     * @param id
     *            the id to set
     */
    protected final void setId(String id) {
        this.id = id;
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
     * @return the language
     */
    public final LanguageContainer getLanguage() {
        return language;
    }

    /**
     * @param language
     *            the language to set
     */
    protected final void setLanguage(LanguageContainer language) {
        this.language = language;
    }

    /**
     * @return the checker
     */
    public final AbstractChecker getChecker() {
        return checker;
    }

    /**
     * @param checker
     *            the checker to set
     */
    protected final void setChecker(AbstractChecker checker) {
        this.checker = checker;
    }
}
