package fr.cnes.analysis.tools.analyzer.datas;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

public interface IChecker {
    /**
     * Run analysis for considering file and rule.
     *
     * @return list of {@link CheckResult}s found during analysis
     * @throws IOException
     *             IO problem occurred
     * @throws JFlexException
     *             JFlex error during analysis
     */
    List<CheckResult> run() throws IOException, JFlexException;

    void setInputFile(File pInputFile) throws FileNotFoundException;

    List<CheckResult> getCheckResults();

    void setCheckResults(List<CheckResult> pCheckResults);

    File getInputFile();
}
