package fr.cnes.analysis.tools.ui.handler;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class RuleAnalysisJob extends Job {
  private Analyzer  analyzer;
  private List<File>      files;
  private List<String>    languageIds;
  private List<String>    excludedIds;
  private List<Violation> violations;

  public RuleAnalysisJob(String name, List<File> files, List<String> languageIds,
      List<String> excludedIds) {
    super(name);
    this.files = files;
    this.languageIds = languageIds;
    this.excludedIds = excludedIds;
    this.analyzer = new Analyzer();
  }

  @Override
  protected IStatus run(IProgressMonitor monitor) {
    IStatus status = Status.OK_STATUS;
    try {
      this.violations = analyzer.applyRules(files, languageIds, excludedIds);
    } catch (FileNotFoundException exception) {
      new Status(Status.ERROR, "fr.cnes.analysis.tools.analyzer", exception.getMessage());
    } catch (Exception exception) {
      new Status(Status.ERROR, "fr.cnes.analysis.tools.analyzer", exception.getMessage());
    }
    return status;
  }

  public Analyzer getAnalyzer() {
    return analyzer;
  }

  public void setAnalyzer(Analyzer analyzer) {
    this.analyzer = analyzer;
  }

  public List<File> getFiles() {
    return files;
  }

  public void setFiles(List<File> files) {
    this.files = files;
  }

  public List<String> getLanguageIds() {
    return languageIds;
  }

  public void setLanguageIds(List<String> languageIds) {
    this.languageIds = languageIds;
  }

  public List<String> getExcludedIds() {
    return excludedIds;
  }

  public void setExcludedIds(List<String> excludedIds) {
    this.excludedIds = excludedIds;
  }

  public List<Violation> getViolations() {
    return violations;
  }

  public void setViolations(List<Violation> violations) {
    this.violations = violations;
  }

}
