/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
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
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;

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

    /** Analyzer extension point identifier */
    public static final String ANALYZER_EP_ID = "fr.cnes.analysis.tools.analyzer";
    /** Analyzer extension's point attribute extension identifier. */
    public static final String ANALYZER_EP_ATTRIBUTE_EXTENSION_ID = "extensionId";
    /** Analyzer extension point element file extension. */
    public static final String ANALYZER_EP_ELEMENT_FILE_EXTENSION = "fileExtension";
    /** Analyzer extension point attribute name for element file extension. */
    public static final String ANALYZER_EP_ELEMENT_FILE_EXTENSION_ATTRIBUTE_NAME = "name";

    /**
     * Identifier attribute's name for every check of contributor of analyzer
     * extension point
     */
    public static final String ANALYZER_EP_CONTRIBUTOR_CHECK_ID = "id";
    /** Logger */
    private static final Logger LOGGER = Logger.getLogger(Analyzer.class.getName());

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
        List<String> languageIds = pLanguageIds;
        if (languageIds == null) {
            languageIds = new ArrayList<>();
            for (IConfigurationElement analyzerContribution : Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(Analyzer.ANALYZER_EP_ID)) {
                languageIds
                        .add(analyzerContribution.getAttribute(ANALYZER_EP_ATTRIBUTE_EXTENSION_ID));
            }
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
         
         */
        for (IConfigurationElement analyzerContribution : Platform.getExtensionRegistry()
                .getConfigurationElementsFor(Analyzer.ANALYZER_EP_ID)) {
            if (languageIds.contains(
                    analyzerContribution.getAttribute(ANALYZER_EP_ATTRIBUTE_EXTENSION_ID))) {
                /*
                 * The current extension is one of the analyzer contribution
                 * that will be run. We are now configuring it.
                 */
                // 1. Setting files that will be analyzed
                // 1.1. Finding allowed extension from the plugin analyzer

                final List<String> allowedExtension = new ArrayList<>();
                for (IConfigurationElement fileExtension : analyzerContribution
                        .getChildren(ANALYZER_EP_ELEMENT_FILE_EXTENSION)) {
                    // we remove the dot in case the contributor added it.
                    if (fileExtension
                            .getAttribute(ANALYZER_EP_ELEMENT_FILE_EXTENSION_ATTRIBUTE_NAME)
                            .contains(".")) {
                        allowedExtension.add(fileExtension
                                .getAttribute(ANALYZER_EP_ELEMENT_FILE_EXTENSION_ATTRIBUTE_NAME)
                                .replace(".", ""));
                    } else {
                        allowedExtension.add(fileExtension
                                .getAttribute(ANALYZER_EP_ELEMENT_FILE_EXTENSION_ATTRIBUTE_NAME));
                    }
                }
                // 1.2. Restricting analysis only on file that the plugin can
                // handle.
                // Note : The restriction is for file without extension and file
                // that will be already
                // analyzed. This is causing crash from the analysis.
                final List<File> restrictedFiles = new ArrayList<>();
                for (File file : pInputFiles) {
                    if (allowedExtension.contains(this.getFileExtension(file.getAbsolutePath()))
                            && !restrictedFiles.contains(file)) {
                        restrictedFiles.add(file);
                    }
                }
                // 2. Running all rules that are not excluded from the analysis.
                // 2.1 Retrieving all rules from the extension point.
                for (IConfigurationElement contribution : Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(analyzerContribution
                                .getAttribute(ANALYZER_EP_ATTRIBUTE_EXTENSION_ID))) {
                    // 2.2 If the rule is not excluded, run the analysis.
                    if (PlatformUI.getPreferenceStore()
                            .contains(contribution.getAttribute(ANALYZER_EP_CONTRIBUTOR_CHECK_ID))
                            && !excludedCheckIds.contains((contribution
                                    .getAttribute(ANALYZER_EP_CONTRIBUTOR_CHECK_ID)))) {
                        AbstractChecker rule;
                        /*
                         * We are currently to load as much Rule as there is
                         * files because the lex files are designed to be run
                         * only on one file.
                         */
                        for (File analyzedFile : restrictedFiles) {
                            try {
                                rule = (AbstractChecker) contribution
                                        .createExecutableExtension("class");
                                rule.setContribution(contribution);
                                final CallableChecker callableAnalysis = new CallableChecker(rule,
                                        analyzedFile);
                                analyzers.add(service.submit(callableAnalysis));
                            } catch (CoreException e) {

                                // TODO : Define how to warn here of the
                                // execution
                                // failure without throwing new
                                // exception
                                e.printStackTrace();
                            }
                        }
                    }
                }
            }
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
