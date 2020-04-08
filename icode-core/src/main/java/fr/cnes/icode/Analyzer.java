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
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.checkers.CheckerContainer;
import fr.cnes.icode.services.checkers.CheckerService;
import fr.cnes.icode.services.languages.ILanguage;
import fr.cnes.icode.services.languages.LanguageService;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <h1>i-Code CNES Analyzer service</h1>
 * <p>
 * This service must be used to run an analysis, using
 * {@link #stableCheck(Set, List, List)} method.
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
     */
    public List<CheckResult> stableCheck(final Set<File> pInputFiles, final List<String> pLanguageIds,
                                   final List<String> pExcludedCheckIds) {
        final String methodName = "stableCheck";
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
                    } catch (final Exception e) {
                        // Set the error message.
                        final String errorMessage = String.format("Internal i-Code error: exception [%s] thrown while checking [%s] on file [%s] - %s",
                                e.getClass().getSimpleName(), checker.getName(), input.getPath(), e.getMessage());
                        // Log the error in i-Code and SonarQube logger.
                        ICodeLogger.error(this.getClass().getName(), methodName, errorMessage);
                        ICodeLogger.error(this.getClass().getName(), methodName, e);
                        // Create an issue to be displayed in SonarQube.
                        final CheckResult exception = new CheckResult("Parsing Error", "Parsing Error", input);
                        exception.setLangageId(language.getId());
                        exception.setLocation("unknown");
                        exception.setLine(0);
                        exception.setMessage(errorMessage);
                        // Add the exception as a Parsing Error result.
                        analysisResultCheckResult.add(exception);
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
