/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.core.runtime.jobs.JobGroup;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * This class can be used to run an analysis from {@link Analyzer} service.
 * <p>
 * On {@link #execute(ExecutionEvent)} :
 * <ul>
 * <li>Identify which languages should be analyzed in {@link IPreferenceStore}.</li>
 * <li>Identify which Rules or Metric should be analyzed in {@link IPreferenceStore}</li>
 * <li>Identify which {@link File}s should be analyzed using {@link PlatformUI}'s selection
 * service.</li>
 * <li>Run an analysis using {@link RuleAnalysisJob}</li>
 * </ul>
 * </p>
 * 
 * @since 3.0
 */
public class AnalysisHandler extends UIAndCommandAbstractHandler {
  /** Logger. **/
  private static final Logger LOGGER = Logger.getLogger(AnalysisHandler.class.getName());

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands. ExecutionEvent)
   */
  @Override
  public Object execute(final ExecutionEvent event) throws ExecutionException {

    /*
     * 1.Identification the languages to analyze :
     * 
     */
    List<String> analysisRules = new ArrayList<>();
    List<String> analysisMetrics = new ArrayList<>();
    IPreferenceStore store = PlatformUI.getPreferenceStore();
    for (IConfigurationElement contribution : Platform.getExtensionRegistry()
        .getConfigurationElementsFor(PreferencesUIUtils.PREF_EXT_PT_ID)) {
      /*
       * 1.1. Identification of the rules plugin activated :
       */
      if (store.getBoolean(contribution.getAttribute(PreferencesUIUtils.PARENT_ID))
          && store.getBoolean(contribution.getAttribute(PreferencesUIUtils.RULE_PAGE_ID))) {
        analysisRules.add(contribution.getAttribute(PreferencesUIUtils.RULE_EXT_ID));
      }
      /*
       * 1.2. Identification of the metric plugin activated :
       */
      if (store.getBoolean(contribution.getAttribute(PreferencesUIUtils.PARENT_ID))
          && store.getBoolean(contribution.getAttribute(PreferencesUIUtils.METRIC_PAGE_ID))) {
        analysisMetrics.add(contribution.getAttribute(PreferencesUIUtils.METRIC_EXT_ID));
      }
    }

    /*
     * 2. Identification the rule & metrics that are deactivated :
     */
    List<String> excludedRules = new ArrayList<>();
    List<String> excludedMetrics = new ArrayList<>();
    for (IConfigurationElement analyzerContribution : Platform.getExtensionRegistry()
        .getConfigurationElementsFor(Analyzer.Analyzer_ExtensionPoint_ID)) {
      if (analysisRules.contains(analyzerContribution.getAttribute("extensionId"))) {
        for (IConfigurationElement contribution : Platform.getExtensionRegistry()
            .getConfigurationElementsFor(analyzerContribution.getAttribute("extensionId"))) {
          if (store.contains(contribution.getAttribute("id"))
              && !store.getBoolean(contribution.getAttribute("id"))) {
            excludedRules.add(contribution.getAttribute("id"));
          }
        }
      } else if (analysisMetrics.contains(analyzerContribution.getAttribute("extensionId"))) {
        for (IConfigurationElement contribution : Platform.getExtensionRegistry()
            .getConfigurationElementsFor(analyzerContribution.getAttribute("extensionId"))) {
          if (store.contains(contribution.getAttribute("id"))
              && !store.getBoolean(contribution.getAttribute("id"))) {
            excludedMetrics.add(contribution.getAttribute("id"));
          }
        }
      }
    }

    /*
     * 3. Identification of the selected files
     */
    List<File> files = new ArrayList<>();
    Iterator selectionIterator = HandlerUtil.getCurrentStructuredSelection(event).iterator();
    while (selectionIterator.hasNext()) {
      Object selection = selectionIterator.next();
      try {
        files.addAll(this.findFiles(selection));
      } catch (CoreException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

    }

    /*
     * 3. Creation of jobs to run analysis.
     */
    RuleAnalysisJob rulesJob = new RuleAnalysisJob("Applying rules...", files, analysisRules,
        excludedRules);
    rulesJob.addJobChangeListener(new JobChangeAdapter() {

      @Override
      public void done(final IJobChangeEvent event) {
        Display.getDefault().asyncExec(new Runnable() {

          @Override
          public void run() {
            if (rulesJob.getResult().isOK()) {
              AnalysisHandler
                  .updateViolationView(((RuleAnalysisJob) event.getJob()).getViolations());
            }
          }

        });
      }
    });
    rulesJob.setUser(true);

    MetricAnalysisJob metricsJob = new MetricAnalysisJob("Computing metrics...", files,
        analysisMetrics, excludedMetrics);
    metricsJob.addJobChangeListener(new JobChangeAdapter() {
      @Override
      public void done(final IJobChangeEvent event) {
        Display.getDefault().asyncExec(new Runnable() {

          @Override
          public void run() {
            if (metricsJob.getResult().isOK()) {
              AnalysisHandler.updateMetricsView(((MetricAnalysisJob) event.getJob()).getMetrics());
            }
          }

        });
      }
    });
    metricsJob.setUser(true);
    JobGroup group = new JobGroup("i-Code CNES Analysis.", 2, 2);
    rulesJob.setJobGroup(group);
    metricsJob.setJobGroup(group);

    metricsJob.schedule();
    rulesJob.schedule();
    return null;
  }

  /**
   * This method can be used to find different File element of a selection.
   * 
   * <p>
   * Warning : this method is recursive
   * </p>
   * 
   * @param selection
   *          The selection to search for files
   * @return a list of file included in the selection
   * @throws CoreException
   *           when resources of a {@link IProject} or {@link IFolder} couldn't be reached
   */
  private List<File> findFiles(Object selection) throws CoreException {
    List<File> files = new ArrayList<>();
    if (selection instanceof IProject) {
      for (Object resource : ((IProject) selection).members()) {
        files.addAll(this.findFiles(resource));
      }
    } else if (selection instanceof IFolder) {
      for (Object resource : ((IFolder) selection).members()) {
        files.addAll(this.findFiles(resource));
      }
    } else if (selection instanceof IFile) {
      files.add(((IFile) selection).getLocation().toFile().getAbsoluteFile());
    }
    return files;
  }

  /**
   * Update the violation's view
   * 
   * @param violations
   *          .
   */
  protected static void updateViolationView(final List<Violation> violations) {
    LOGGER.finest("Begin updateView method");

    try {
      // get the page
      final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
          .getActivePage();

      // open view
      page.showView(ViolationsView.VIEW_ID);

      // get view
      final ViolationsView view = (ViolationsView) page.findView(ViolationsView.VIEW_ID);

      // show rules analyze results
      if (view != null) {
        view.display(violations);
      }

    } catch (final PartInitException exception) {
      LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
      showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Internal Error",
          "Contact support service : \n" + exception.getMessage());
    }

    LOGGER.finest("End updateView method");
  }

  private static void updateMetricsView(final List<FileValue> values) {
    LOGGER.finest("Begin updateView method");

    try {
      // get the page
      final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
          .getActivePage();

      // open view
      page.showView(MetricsView.VIEW_ID);

      // get view
      final MetricsView view = (MetricsView) page.findView(MetricsView.VIEW_ID);

      // show rules analyze results
      if (view != null) {
        view.display(values);
        /* Add IMarkers everywhere there is a metric event */
        view.insertMarkers();
      }

    } catch (final PartInitException exception) {
      LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
      showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Internal Error",
          "Contact support service : \n" + exception.getMessage());
    } catch (final EmptyProviderException exception) {
      LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
      showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Internal Error",
          "Contact support service : \n" + exception.getMessage());
    }

    LOGGER.finest("End updateView method");
  }

}
