/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.checkers.CheckerContainer;
import fr.cnes.icode.services.checkers.CheckerService;
import fr.cnes.icode.services.languages.ILanguage;
import fr.cnes.icode.services.languages.LanguageService;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * <h1>i-Code CNES Analyzer service</h1>
 * <p>
 * This service must be used to run an analysis, using
 * {@link #check(Set, List, List)} method.
 * </p>
 * <p>
 * To reach required parameters, several services can be used :
 * <ul>
 * <li><code>languagesIds</code> - {@link LanguageService};</li>
 * <li><code>excludedIds</code> - {@link CheckerService};</li>
 * </ul>
 * 
 * <p>
 * For more information on how to contribute to this, please refer to i-Code
 * CNES User Manual.
 * </p>
 * 
 * @since 3.0
 *
 */
public class Analyzer {

    /** Number of thread to run the analysis */
    private static final int THREAD_NB = Runtime.getRuntime().availableProcessors();

    /**
     * Define ratio of max memory allocated to the JVM to use for the analysis.
     */
    private static final double MAX_MEMORY_THRESHOLD = 0.90;

    /**
     * <h1>{@link #check(Set, List, List)}</h1>
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
     * @throws JFlexException
     *             when the syntax analysis failed.
     */
    public List<CheckResult> check(final Set<File> pInputFiles, final List<String> pLanguageIds,
                                   final List<String> pExcludedCheckIds) throws JFlexException {
        final String methodName = "check";
        ICodeLogger.entering(this.getClass().getName(), methodName);

        List<String> languageIds = pLanguageIds;
        if (languageIds == null) {
            languageIds = LanguageService.getLanguagesIds();
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
         */
        final ExecutorService service = Executors.newFixedThreadPool(THREAD_NB);
        final List<Future<List<CheckResult>>> analyzers = new ArrayList<>();

        // Contains checkers by language.
        final Map<String,List<CheckerContainer>> checkers = Maps.newHashMap();
        // Contains files by language.
        final Map<String, List<File>> inputs = Maps.newLinkedHashMap();

        try {
            // Get max memory for memory cleaning purpose.
            final long maxMemory = Runtime.getRuntime().maxMemory();
            // Get languages to check during the analysis.
            final List<ILanguage> languages = LanguageService.getLanguages(languageIds);
            // Get checkers to run during analysis.
            for(final ILanguage language : languages) {
                checkers.put(language.getId(), CheckerService.getCheckers(language.getId(), excludedCheckIds));
            }

            // Sort files by language.
            for (final File file : pInputFiles) {
                final String languageId = LanguageService.getLanguageId(getFileExtension(file.getAbsolutePath()));
                final List<File> tempList = inputs.getOrDefault(languageId, Lists.newArrayList());
                tempList.add(file);
                inputs.put(languageId, tempList);
            }
            // For each selected language, run selected checkers on selected files.
            for(final ILanguage language : languages) {
                for (final File input : inputs.getOrDefault(language.getId(), Lists.newArrayList())) {
                    for (final CheckerContainer checker : checkers.get(language.getId())) {
                        final CallableChecker callableAnalysis = new CallableChecker(
                                checker.getChecker(), input);
                        if ((MAX_MEMORY_THRESHOLD * maxMemory < (Runtime.getRuntime().totalMemory()
                                - Runtime.getRuntime().freeMemory()))
                                && !analyzers.isEmpty()) {
                            for (final Future<List<CheckResult>> analysis : analyzers) {
                                analysisResultCheckResult.addAll(analysis.get());
                            }
                            analyzers.clear();
                        }
                        analyzers.add(service.submit(callableAnalysis));
                    }
                }
            }
            for (Future<List<CheckResult>> analysis : analyzers) {
                analysisResultCheckResult.addAll(analysis.get());
            }
        } catch (final InterruptedException e) {
            ICodeLogger.error(this.getClass().getName(), methodName, e);
        } catch (final ExecutionException exception) {
            ICodeLogger.error(this.getClass().getName(), methodName, exception);
            if (exception.getCause() instanceof JFlexException) {
                ICodeLogger.throwing(this.getClass().getName(), methodName, exception.getCause());
                throw (JFlexException) exception.getCause();
            }
        }
        return analysisResultCheckResult;
    }

    /**
     * <h1>{@link #stableCheck(Set, List, List)}</h1>
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
     * @throws JFlexException
     *             when the syntax analysis failed.
     */
    public List<CheckResult> stableCheck(final Set<File> pInputFiles, final List<String> pLanguageIds,
                                   final List<String> pExcludedCheckIds) throws JFlexException {
        final String methodName = "check";
        ICodeLogger.entering(this.getClass().getName(), methodName);

        List<String> languageIds = pLanguageIds;
        if (languageIds == null) {
            languageIds = LanguageService.getLanguagesIds();
        }
        List<String> excludedCheckIds = pExcludedCheckIds;
        if (pExcludedCheckIds == null) {
            excludedCheckIds = new ArrayList<>();
        }
        final List<CheckResult> analysisResultCheckResult = new ArrayList<>();

        // Contains checkers by language.
        final Map<String,List<CheckerContainer>> checkers = Maps.newHashMap();
        // Contains files by language.
        final Map<String, List<File>> inputs = Maps.newLinkedHashMap();
        // Get languages to check during the analysis.
        final List<ILanguage> languages = LanguageService.getLanguages(languageIds);
        // Get checkers to run during analysis.
        for(final ILanguage language : languages) {
            checkers.put(language.getId(), CheckerService.getCheckers(language.getId(), excludedCheckIds));
        }

        // Sort files by language.
        for (final File file : pInputFiles) {
            final String languageId = LanguageService.getLanguageId(getFileExtension(file.getAbsolutePath()));
            final List<File> tempList = inputs.getOrDefault(languageId, Lists.newArrayList());
            tempList.add(file);
            inputs.put(languageId, tempList);
        }
        // For each selected language, run selected checkers on selected files.
        for (final ILanguage language : languages) {
            for (final File input : inputs.getOrDefault(language.getId(), Lists.newArrayList())) {
                for (final CheckerContainer checker : checkers.get(language.getId())) {
                    try {
                        AbstractChecker check = checker.getChecker();
                        check.setInputFile(input);
                        analysisResultCheckResult.addAll(check.run());
                    } catch (final IOException e) {
                        ICodeLogger.error(this.getClass().getName(), methodName, e);
                    }
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
    private String getFileExtension(final String pFileName) {
        String extension = null;

        final int i = pFileName.lastIndexOf('.');
        final int p = Math.max(pFileName.lastIndexOf('/'), pFileName.lastIndexOf('\\'));

        if (i > p) {
            extension = pFileName.substring(i + 1);
        }
        return extension;
    }

}
