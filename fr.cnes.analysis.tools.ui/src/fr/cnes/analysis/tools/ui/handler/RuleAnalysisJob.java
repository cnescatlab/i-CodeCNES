package fr.cnes.analysis.tools.ui.handler;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
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
 * This {@link Job} run a rule analysis using {@link Analyzer} service.
 * 
 * @since 3.0
 */
public class RuleAnalysisJob extends Job {

    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(RuleAnalysisJob.class.getName());

    /** {@link Analyzer} service to run the analysis */
    private Analyzer analyzer;
    /** List of files to analyze. */
    private List<File> inputFiles;
    /** List of languages plugin identifiers to run analysis with */
    private List<String> languageIds;
    /**
     * List of all rules excluded from the analysis. <i>More informations on
     * :</i> {@link Analyzer#check(List, List, List)}
     */
    private List<String> excludedIds;
    /**
     * {@link CheckResult} list from analysis result.
     */
    private List<CheckResult> violations;

    /**
     * Constructor for {@link RuleAnalysisJob}
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
    public RuleAnalysisJob(String pName, List<File> pInputFiles, List<String> pLanguageIds,
            List<String> pExcludedIds) {
        super(pName);
        this.inputFiles = pInputFiles;
        this.languageIds = pLanguageIds;
        this.excludedIds = pExcludedIds;
        this.analyzer = new Analyzer();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        IStatus status = Status.OK_STATUS;
        try {
            this.violations = analyzer.check(inputFiles, languageIds, excludedIds);
        } catch (IOException | JFlexException exception) {
            LOGGER.info(
                    exception.getClass().getName() + " received. Setting job status to warning.");
            status = new Status(IStatus.ERROR, Analyzer.ANALYZER_EP_ID, exception.getMessage());
        }
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
     * @return {@link CheckResult} list from analysis result
     */
    public List<CheckResult> getCheckResults() {
        return violations;
    }

}
