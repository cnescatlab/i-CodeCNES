/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * This {@link Job} run an analysis using {@link Analyzer} service.
 * 
 * @since 3.0
 */
public class AnalysisJob extends Job {

    /** Class name */
    private static final String CLASS = AnalysisJob.class.getName();

    /** {@link Analyzer} service to run the analysis */
    private Analyzer analyzer;
    /** List of files to analyze. */
    private List<File> inputFiles;
    /** List of languages plug-in identifiers to run analysis with */
    private List<String> languageIds;
    /**
     * List of all metrics excluded from the analysis. <i>More informations on
     * :</i> {@link Analyzer#computeMetrics(List, List, List)}
     */
    private List<String> excludedIds;
    /**
     * {@link FileValue} list from analysis result.
     */
    private List<CheckResult> checks;

    /**
     * Constructor for {@link AnalysisJob}
     * 
     * @param pName
     *            name of the Job
     * @param pInputFiles
     *            to analyze
     * @param pLanguageIds
     *            to run analysis with
     * @param pExcludedIds
     *            to exclude from analysis
     */
    public AnalysisJob(final String pName, final List<File> pInputFiles,
                    final List<String> pLanguageIds, final List<String> pExcludedIds) {
        super(pName);
        final String method = "AnalysisJob";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pName, pInputFiles, pLanguageIds, pExcludedIds
        });
        this.inputFiles = pInputFiles;
        this.languageIds = pLanguageIds;
        this.excludedIds = pExcludedIds;
        this.analyzer = new Analyzer();
        ICodeLogger.exiting(CLASS, method);
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        final String method = "run";
        ICodeLogger.entering(this.getClass().getName(), method, monitor);
        IStatus status = Status.OK_STATUS;
        monitor.setTaskName("Analyzing files...");
        try {
            this.checks = analyzer.check(inputFiles, languageIds, excludedIds);
        } catch (IOException | JFlexException exception) {
            ICodeLogger.warning(CLASS, method, exception.getClass().getName() + " handled in method "
                            + method + " changing Job status.");
            status = new Status(IStatus.ERROR, Analyzer.ANALYZER_PLUGIN_ID, exception.getMessage());
        }
        ICodeLogger.exiting(CLASS, method, status);
        return status;
    }

    /**
     * @return files to analyzed
     */
    public List<File> getInputFiles() {
        final String method = "getInputFiles";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, inputFiles);
        return inputFiles;
    }

    /**
     * @param pInputFiles
     *            to analyze
     */
    public void setInputFiles(List<File> pInputFiles) {
        final String method = "setInputFiles";
        ICodeLogger.entering(CLASS, method, pInputFiles);
        this.inputFiles = pInputFiles;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return checkerResults results from the analysis.
     */
    public List<CheckResult> getCheckResults() {
        final String method = "getCheckResults";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checks);
        return checks;
    }

}
