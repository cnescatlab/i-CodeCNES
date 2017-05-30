/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
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
public class CallableRuleAnalyzer implements Callable<List<CheckResult>> {

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
    public List<CheckResult> call() throws IOException, JFlexException {
        final List<CheckResult> results = new ArrayList<>();
        rule.setInputFile(file);
        results.addAll(rule.run());
        return results;
    }

}
