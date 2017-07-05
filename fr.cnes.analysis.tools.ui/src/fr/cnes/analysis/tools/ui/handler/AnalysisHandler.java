/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.markers.InformationMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.core.runtime.jobs.JobGroup;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * This class can run analysis using {@link Analyzer} service.
 * <p>
 * On {@link #execute(ExecutionEvent)} :
 * <ul>
 * <li>Identify which languages should be analyzed in
 * {@link IPreferenceStore}.</li>
 * <li>Identify which Rules or Metric should be analyzed in
 * {@link IPreferenceStore}</li>
 * <li>Identify which {@link File}s should be analyzed using
 * {@link PlatformUI}'s selection service.</li>
 * <li>Run an analysis using {@link RuleAnalysisJob}</li>
 * </ul>
 * </p>
 * 
 * @since 3.0
 */
public class AnalysisHandler extends UIAndCommandAbstractHandler {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(AnalysisHandler.class.getName());
    /** Logger method */
    private static String METHOD = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        METHOD = "execute";
        LOGGER.entering(this.getClass().getName(), METHOD);
        /*
         * 1.Identification the languages to analyze :
         * 
         */
        final List<String> languagesIds = UserPreferencesService.getEnabledLanguagesIds();
        final List<String> excludedChecksIds = UserPreferencesService.getDisabledCheckersIds();

        /*
         * 3. Identification of the selected files
         */
        List<File> files = new ArrayList<>();
        try {
            files = retrieveSelectedFiles(HandlerUtil.getCurrentStructuredSelection(event));

            /*
             * 4. Creation of jobs to run analysis.
             */
            final AnalysisJob rulesJob = new AnalysisJob("Running analysis...", files, languagesIds,
                    excludedChecksIds);
            rulesJob.addJobChangeListener(new JobChangeAdapter() {

                @Override
                public void done(final IJobChangeEvent event) {
                    Display.getDefault().asyncExec(new Runnable() {

                        @Override
                        public void run() {
                            if (rulesJob.getResult().isOK()) {
                                List<CheckResult> results = ((AnalysisJob) event.getJob())
                                        .getCheckResults();
                                List<CheckResult> resultsViolation = new ArrayList<>();
                                List<CheckResult> resultsMetric = new ArrayList<>();
                                for (CheckResult result : results) {
                                    if (result.getValue() == null) {
                                        resultsViolation.add(result);
                                    } else {
                                        resultsMetric.add(result);
                                    }
                                }
                                AnalysisHandler.updateViolationsView(resultsViolation);
                                AnalysisHandler.updateMetricsView(resultsMetric);
                                try {
                                    AnalysisHandler.insertMarkers(results);
                                } catch (InvocationTargetException | InterruptedException e) {
                                    // TODO Auto-generated catch block
                                    e.printStackTrace();
                                }
                            }
                        }
                    });
                }
            });

            final JobGroup group = new JobGroup("i-Code CNES Analysis.", 2, 2);
            rulesJob.setJobGroup(group);

            // Launching the analysis.
            rulesJob.schedule();
        } catch (EmptySelectionException | CoreException exception) {
            MessageDialog.openWarning(HandlerUtil.getActiveShell(event), "Core exception",
                    exception.getMessage());
        }
        LOGGER.exiting(this.getClass().getName(), METHOD, null);
        return null;
    }

    /**
     * This method return {@link File}s in UI selected by the user.
     * 
     * @param pSelection
     *            currently selected by the user on UI
     * @return every {@link File}s in the selection
     * @throws EmptySelectionException
     *             when no selection is set.
     * @throws CoreException
     *             when some resources are not reachable.
     */
    private List<File> retrieveSelectedFiles(IStructuredSelection pSelection)
            throws EmptySelectionException, CoreException {
        METHOD = "retrieveSelectedFiles";
        LOGGER.entering(this.getClass().getName(), METHOD, pSelection);
        final List<File> files = new ArrayList<>();
        final Iterator<IResource> selectionIterator = pSelection.iterator();
        if (!selectionIterator.hasNext()) {
            throw new EmptySelectionException("Erreur d'exÃ©cution (EmptySelectionException).");
        }
        while (selectionIterator.hasNext()) {
            final IResource selection = selectionIterator.next();
            files.addAll(this.findFiles(selection));
        }
        LOGGER.exiting(this.getClass().getName(), METHOD, files);
        return files;

    }

    /**
     * This method can be used to find different File element of a selection.
     * 
     * <p>
     * <strong>Warning</strong> : this method is recursive
     * </p>
     * 
     * @param selection
     *            The selection to search for files
     * @return a list of file included in the selection
     * @throws CoreException
     *             when resources of a {@link IProject} or {@link IFolder}
     *             couldn't be reached
     */
    private List<File> findFiles(IResource selection) throws CoreException {
        METHOD = "findFiles";
        LOGGER.entering(this.getClass().getName(), METHOD);
        final List<File> files = new ArrayList<>();
        switch (selection.getType()) {
            case IResource.ROOT:
                for (IResource resource : ((IWorkspaceRoot) selection).members()) {
                    files.addAll(this.findFiles(resource));
                }
                break;
            case IResource.PROJECT:
                for (IResource resource : ((IProject) selection).members()) {
                    files.addAll(this.findFiles(resource));
                }
                break;
            case IResource.FOLDER:
                for (IResource resource : ((IFolder) selection).members()) {
                    files.addAll(this.findFiles(resource));
                }
                break;
            case IResource.FILE:
                files.add(((IFile) selection).getLocation().toFile().getAbsoluteFile());
                break;
            default:
                break;
        }
        LOGGER.exiting(this.getClass().getName(), METHOD, files);
        return files;
    }

    /**
     * Update the violation's view
     * 
     * @param violations
     *            to show in the view.
     */
    protected static void updateViolationsView(final List<CheckResult> violations) {
        METHOD = "updateCheckResultView";
        LOGGER.entering(AnalysisHandler.class.getName(), METHOD, violations);

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
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.exiting(AnalysisHandler.class.getName(), METHOD);
    }

    /**
     * Update MetricsView
     * 
     * @param values
     *            to show in the view
     */
    private static void updateMetricsView(final List<CheckResult> values) {
        METHOD = "updateMetricsView";
        LOGGER.entering(AnalysisHandler.class.getName(), METHOD, values);

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
            }

        } catch (final PartInitException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final EmptyProviderException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.exiting(AnalysisHandler.class.getName(), METHOD);
    }

    /**
     * This method insert for each violation detected a new marker on the line
     * of the violation.
     * 
     * @param checks
     *            the checks to add marker with
     * @throws InterruptedException
     * @throws InvocationTargetException
     */
    public static void insertMarkers(List<CheckResult> checks)
            throws InvocationTargetException, InterruptedException {
        LOGGER.finest("begin method insertMarkers");
        final ProgressMonitorDialog pmdialog = new ProgressMonitorDialog(
                PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
        pmdialog.run(true, true, new WorkspaceModifyOperation() {

            @Override
            protected void execute(final IProgressMonitor monitor)
                    throws CoreException, InvocationTargetException, InterruptedException {
                // create all my markers here

                try {

                    final HashSet<IFile> cleanedFiles = new HashSet<IFile>();
                    String ruleName, severity, message = "Violation detected here.";
                    Integer line;
                    IFile file;

                    for (final CheckResult check : checks) {
                        file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(
                                new Path(check.getFile().getAbsolutePath()).makeRelativeTo(
                                        ResourcesPlugin.getWorkspace().getRoot().getFullPath()));
                        if (file != null && file.exists()) {
                            // If the file already has marker of type violations
                            // then we clean the file once
                            if (file != null && !cleanedFiles.contains(file)) {
                                cleanedFiles.add(file);
                                file.deleteMarkers(
                                        "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker",
                                        true, 1);
                                file.deleteMarkers(
                                        "fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker",
                                        true, 1);
                                file.deleteMarkers(
                                        "fr.cnes.analysis.tools.ui.markers.InformationMarker", true,
                                        1);
                            }
                        }

                        Float limit = Float.NaN;
                        boolean violation = false;

                        if (UserPreferencesService.hasMaxValue(check.getId())
                                && !UserPreferencesService.getMaxValue(check.getId()).isNaN()) {
                            limit = UserPreferencesService.getMaxValue(check.getId());
                            violation = check.getValue().compareTo(limit) > 0;
                            message = violation
                                    ? check.getName() + " | Value is " + check.getValue()
                                            + " while it should not exceed " + limit + "."
                                    : check.getName() + " | Value is " + check.getValue()
                                            + ", below it's maximum limit of " + limit + ".";
                        } else if (UserPreferencesService.hasMinValue(check.getId())
                                && !UserPreferencesService.getMaxValue(check.getId()).isNaN()) {
                            limit = UserPreferencesService.getMinValue(check.getId());
                            violation = check.getValue().compareTo(limit) < 0;
                            message = violation
                                    ? check.getName() + " | Value is " + check.getValue()
                                            + " while it should not below  " + limit + "."
                                    : check.getName() + " | Value is " + check.getValue()
                                            + ", above it's minimum limit of  " + limit + ".";
                        } else {
                            if (check.getMessage() == null || check.getMessage().isEmpty()) {
                                if (check.getValue() != null && !check.getValue().isNaN()) {
                                    message = check.getName() + " | Value is " + check.getValue()
                                            + ".";
                                } else {
                                    message = check.getName()
                                            + " | No message in description. Please refer to CNES"
                                            + " RNC.";
                                    violation = true;
                                }
                            } else {
                                message = check.getName() + " | " + check.getMessage();
                                violation = true;
                            }

                        }

                        if (violation && UserPreferencesService.getCheckerSeverity(check.getId())
                                .equals(UserPreferencesService.PREF_SEVERITY_ERROR_VALUE)) {
                            ViolationErrorMarker.createMarker(file, check.getLine(),
                                    check.getName(), message);
                        } else if (violation
                                && UserPreferencesService.getCheckerSeverity(check.getId()).equals(
                                        UserPreferencesService.PREF_SEVERITY_WARNING_VALUE)) {
                            ViolationWarningMarker.createMarker(file, check.getLine(),
                                    check.getName(), message);
                        } else {
                            InformationMarker.createMarker(file, check.getLine(), check.getName(),
                                    message);
                        }
                    }
                } catch (final CoreException exception) {
                    LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
                    MessageDialog.openError(
                            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Marker problem", exception.getMessage());

                }
            }
        });
        // One time all markers have been insert, we refresh all
        // decorators.

        final IDecoratorManager manager = PlatformUI.getWorkbench().getDecoratorManager();

        manager.update("fr.cnes.analysis.tools.ui.decorators.violationwarningdecorator");
        manager.update("fr.cnes.analysis.tools.ui.decorators.violationerrordecorator");
        manager.update("fr.cnes.analysis.tools.ui.decorators.informationdecorator");

        LOGGER.finest("end method insertMarkers");

    }

}
