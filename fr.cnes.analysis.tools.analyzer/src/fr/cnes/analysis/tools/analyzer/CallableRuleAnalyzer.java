package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * This class is responsible of applying a rule on a file and to return it's
 * results as a Thread by implementing {@link Callable} interface.
 * 
 * @since 3.0
 */
public class CallableRuleAnalyzer implements Callable<List<Violation>> {

    /** The rule to apply */
    private AbstractRule rule;
    /** The metric to analyze */
    private File file;

    /**
     * Constructor for {@link CallableRuleAnalyzer}.
     * 
     * @param pRule
     *            to apply
     * @param pInputFile
     *            to analyze
     */
    public CallableRuleAnalyzer(AbstractRule pRule, File pInputFile) {
        this.rule = pRule;
        this.file = pInputFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Callable#call()
     */
    @Override
    public List<Violation> call() throws IOException, JFlexException {
        final List<Violation> results = new ArrayList<>();
        rule.setInputFile(file);
        results.addAll(rule.run());
        return results;
    }

}
