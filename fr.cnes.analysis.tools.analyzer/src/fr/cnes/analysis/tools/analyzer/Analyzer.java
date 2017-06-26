/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerContainer;
import fr.cnes.analysis.tools.analyzer.services.checkers.CheckerService;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Logger;
import org.eclipse.core.runtime.CoreException;

/**
 * <h1>i-Code CNES analyzer service</h1>
 * <p>
 * This class can be used by any third using {@link File} and {@link String} to
 * run an analysis.
 * </p>
 * <p>
 * <h2>Available methods</h2> The service method to call to run an analysis are
 * {@link #check(List, List, List)} and
 * {@link #computeMetrics(List, List, List)}. Once, it returns after a moment
 * the results thanks to {@link CallableMetricAnalyzer} &
 * {@link CallableChecker}.
 * </p>
 * <h2>Number of threads</h2>
 * <p>
 * To define the number of threads that should be running the analysis change
 * the parameter {@link #THREAD_NB}. It's <strong>default value</strong> is set
 * on <strong>one</strong>.
 * </p>
 * <h2>Handling exceptions</h2>
 * <p>
 * Both analyzer can throw the following type of exceptions :
 * <ul>
 * <li>{@link JFlexException} : When the syntax analysis is interrupted because
 * JFLex couldn't handle the file. This can be thrown because of <i>file
 * encryption, file format, unexpected file state</i> problem.</li>
 * <li>{@link FileNotFoundException} : When one of the file couldn't be reached
 * by the analyzer.</li>
 * </ul>
 * </p>
 * 
 * @since 3.0
 *
 */
public class Analyzer {

    /** Logger */
    private static final Logger LOGGER = Logger.getLogger(Analyzer.class.getName());

    /** Analyzer plugin ID */
    public static final String ANALYZER_PLUGIN_ID = "fr.cnes.analysis.tools.analyzer";

    /** Number of thread to run the analysis */
    private static int THREAD_NB = 1;

    /**
     * <h1>{@link #check(List, List, List)}</h1>
     * <p>
     * This method apply all rules of the different contributions set in
     * parameter except the one excluded. File in parameters are being analyzed
     * by each contribution able to handle it or none if it isn't.
     * </p>
     * <p>
     * <strong>Important :</strong> Default configurations to run analysis are
     * available when setting parameters.
     * 
     * @param pInputFiles
     *            to analyze
     * @param pLanguageIds
     *            to include in the analysis. <strong>Set null</strong> to run
     *            an analysis including all contributions.
     * @param pExcludedCheckIds
     *            rules identifier to exclude from the analysis. <strong>Set
     *            null</strong> run analysis with every rules.
     * @return list of {@link CheckResult} found by the analysis.
     * @throws IOException
     *             when a file couldn't be reached for analysis.
     * @throws JFlexException
     *             when the syntax analysis failed.
     */
    public List<CheckResult> check(List<File> pInputFiles, List<String> pLanguageIds,
            List<String> pExcludedCheckIds) throws IOException, JFlexException {
        final String methodName = "check";
        LOGGER.entering(this.getClass().getName(), methodName);

        // Running both checker & language services.
        CheckerService checkerService = new CheckerService();
        LanguageService languageService = new LanguageService();

        List<String> languageIds = pLanguageIds;
        if (languageIds == null) {
            languageIds = languageService.getLanguagesIds();
        }
        List<String> excludedCheckIds = pExcludedCheckIds;
        if (pExcludedCheckIds == null) {
            excludedCheckIds = new ArrayList<>();
        }
        final List<CheckResult> analysisResultCheckResult = new ArrayList<>();
        /*
         * The number of threads could be defined by the number of files or the
         * number of rule or both of them. This is pending how we decide to run
         * the analysis.
         * 
         * TODO : Chose one solution for the number of threads
         */
        final ExecutorService service = Executors.newFixedThreadPool(THREAD_NB);
        final List<Future<List<CheckResult>>> analyzers = new ArrayList<Future<List<CheckResult>>>();

        /*
         * Each language must be run with it's own files (pending file
         * extension).
         */
        List<CheckerContainer> checkers;

        try {
            checkers = checkerService.getCheckers(languageIds, excludedCheckIds);
            for (CheckerContainer checker : checkers) {
                for (File file : pInputFiles) {
                    if (checker.canVerifyFormat(this.getFileExtension(file.getAbsolutePath()))) {
                        final CallableChecker callableAnalysis = new CallableChecker(
                                checker.getChecker(), file);
                        analyzers.add(service.submit(callableAnalysis));
                    }
                }
            }
        } catch (NullContributionException | CoreException e) {
            /*
             * TODO : Define how to handles theses cases.
             * 
             */
            e.printStackTrace();
        }

        for (Future<List<CheckResult>> analysis : analyzers) {
            try {
                analysisResultCheckResult.addAll(analysis.get());
            } catch (InterruptedException interruptedException) {
                LOGGER.throwing(this.getClass().getName(), methodName, interruptedException);
            } catch (ExecutionException executionException) {
                if (executionException.getCause() instanceof IOException) {
                    throw ((IOException) executionException.getCause());
                } else if (executionException.getCause() instanceof JFlexException) {
                    throw ((JFlexException) executionException.getCause());
                } else {
                    executionException.printStackTrace();
                }
            }
        }
        return analysisResultCheckResult;
    }

    /**
     * 
     * @param pFileName
     *            to retrieve the extension
     * @return The extension name of the file
     */
    private String getFileExtension(String pFileName) {
        String extension = null;

        final int i = pFileName.lastIndexOf('.');
        final int p = Math.max(pFileName.lastIndexOf('/'), pFileName.lastIndexOf('\\'));

        if (i > p) {
            extension = pFileName.substring(i + 1);
        }
        return extension;
    }

}
