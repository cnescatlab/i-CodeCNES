package fr.cnes.analysis.tools.ui.handler;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * This {@link Job} run a metric analysis using {@link Analyzer} service.
 * 
 * @since 3.0
 */
public class MetricAnalysisJob extends Job {
    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(RuleAnalysisJob.class.getName());
    /** Logger's method identifier */
    private static String METHOD = "";
    /** {@link Analyzer} service to run the analysis */
    private Analyzer analyzer;
    /** List of files to analyze. */
    private List<File> inputFiles;
    /** List of languages plugin identifiers to run analysis with */
    private List<String> languageIds;
    /**
     * List of all metrics excluded from the analysis. <i>More informations on
     * :</i> {@link Analyzer#computeMetrics(List, List, List)}
     */
    private List<String> excludedIds;
    /**
     * {@link FileValue} list from analysis result.
     */
    private List<FileValue> metrics;

    /**
     * Constructor for {@link MetricAnalysisJob}
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
    public MetricAnalysisJob(String pName, List<File> pInputFiles, List<String> pLanguageIds,
            List<String> pExcludedIds) {
        super(pName);
        this.inputFiles = pInputFiles;
        this.languageIds = pLanguageIds;
        this.excludedIds = pExcludedIds;
        this.analyzer = new Analyzer();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        METHOD = "run";
        LOGGER.entering(this.getClass().getName(), METHOD, monitor);
        IStatus status = Status.OK_STATUS;
        try {
            this.metrics = analyzer.computeMetrics(inputFiles, languageIds, excludedIds);
        } catch (IOException | JFlexException exception) {
            LOGGER.info(exception.getClass() + " handled in method " + METHOD
                    + " changing Job status.");
            status = new Status(IStatus.ERROR, Analyzer.ANALYZER_EP_ID, exception.getMessage());
        }
        LOGGER.exiting(this.getClass().getName(), METHOD, monitor);
        return status;
    }

    /**
     * @return files to analyzed
     */
    public List<File> getInputFiles() {
        return inputFiles;
    }

    /**
     * @param pInputFiles
     *            to analyze
     */
    public void setInputFiles(List<File> pInputFiles) {
        this.inputFiles = pInputFiles;
    }

    /**
     * @return metrics results from the analysis.
     */
    public List<FileValue> getMetrics() {
        return metrics;
    }

}
