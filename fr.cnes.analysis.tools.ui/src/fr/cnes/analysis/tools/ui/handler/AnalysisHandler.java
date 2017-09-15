/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
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

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.decorators.InformationDecorator;
import fr.cnes.analysis.tools.ui.decorators.ViolationErrorDecorator;
import fr.cnes.analysis.tools.ui.decorators.ViolationWarningDecorator;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.markers.InformationMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;

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
public class AnalysisHandler extends AbstractHandler {

    /** Class name */
    private static final String CLASS = AnalysisHandler.class.getName();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final String method = "execute";
        ICodeLogger.entering(CLASS, method);
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
            final AnalysisJob analysisJob = new AnalysisJob("Running analysis...", files,
                            languagesIds, excludedChecksIds);
            analysisJob.setUser(true);
            analysisJob.addJobChangeListener(new JobChangeAdapter() {

                @Override
                public void done(final IJobChangeEvent event) {
                    Display.getDefault().asyncExec(new Runnable() {

                        @Override
                        public void run() {
                            if (analysisJob.getResult().isOK()) {
                                final List<CheckResult> results = ((AnalysisJob) event.getJob())
                                                .getCheckResults();
                                final List<CheckResult> resultsViolation = new ArrayList<>();
                                final List<CheckResult> resultsMetric = new ArrayList<>();
                                for (CheckResult result : results) {
                                    if (result.getValue() == null) {
                                        resultsViolation.add(result);
                                    } else {
                                        resultsMetric.add(result);
                                    }
                                }
                                AnalysisHandler.updateViolationsView(resultsViolation);
                                AnalysisHandler.updateMetricsView(resultsMetric);
                                AnalysisHandler.insertMarkers(results);
                            } else {
                                ICodeLogger.error(CLASS, method,
                                                analysisJob.getResult().getMessage());
                                MessageDialog.openError(
                                                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                                                .getShell(),
                                                "i-Code CNES - Analysis failure",
                                                analysisJob.getResult().getMessage());
                            }
                        }
                    });
                }
            });

            final JobGroup group = new JobGroup("i-Code CNES Analysis.", 2, 2);
            analysisJob.setJobGroup(group);

            // Launching the analysis.
            analysisJob.schedule();

        } catch (EmptySelectionException exception) {
            ICodeLogger.warning(CLASS, method, exception);
            MessageDialog.openWarning(HandlerUtil.getActiveShell(event), "i-Code CNES - Warning",
                            exception.getMessage());
        } catch (CoreException exception) {
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(HandlerUtil.getActiveShell(event), "i-Code CNES - ERROR",
                            exception.getMessage());
        }
        ICodeLogger.exiting(CLASS, method, null);
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
        final String method = "retrieveSelectedFiles";
        ICodeLogger.entering(CLASS, method, pSelection);
        final List<File> files = new ArrayList<>();
        final Iterator<?> selectionIterator = pSelection.iterator();
        if (!selectionIterator.hasNext()) {
            final EmptySelectionException exception = new EmptySelectionException(
                            "i-Code CNES : Please select file(s) in the Project"
                                            + " Explorer before running an analysis.");
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }
        while (selectionIterator.hasNext()) {
            final IResource selection = (IResource) selectionIterator.next();
            files.addAll(this.findFiles(selection));
        }
        ICodeLogger.exiting(CLASS, method, files);
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
        final String method = "findFiles";
        ICodeLogger.entering(CLASS, method);
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
        ICodeLogger.exiting(CLASS, method, files);
        return files;
    }

    /**
     * Update the violation's view
     * 
     * @param violations
     *            to show in the view.
     */
    protected static void updateViolationsView(final List<CheckResult> violations) {
        final String method = "updateCheckResultView";
        ICodeLogger.entering(CLASS, method, violations);

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
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Update MetricsView
     * 
     * @param values
     *            to show in the view
     */
    private static void updateMetricsView(final List<CheckResult> values) {
        final String method = "updateMetricsView";
        ICodeLogger.entering(CLASS, method, values);

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
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        } catch (final EmptyProviderException exception) {
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * This method insert for each violation detected a new marker on the line
     * of the violation.
     * 
     * @param checks
     *            the checks to add marker with
     */
    public static void insertMarkers(List<CheckResult> checks) {
        final String method = "insertMarkers";
        ICodeLogger.entering(CLASS, method, checks);
        final ProgressMonitorDialog pmdialog = new ProgressMonitorDialog(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
        try {
            pmdialog.run(true, true, new WorkspaceModifyOperation() {
                @Override
                protected void execute(final IProgressMonitor monitor) throws CoreException,
                                InvocationTargetException, InterruptedException {
                    try {

                        final HashSet<IFile> cleanedFiles = new HashSet<IFile>();
                        String message = "Violation detected here.";
                        IFile file;
                        for (final CheckResult check : checks) {
                            file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(
                                            new Path(check.getFile().getAbsolutePath())
                                                            .makeRelativeTo(ResourcesPlugin
                                                                            .getWorkspace()
                                                                            .getRoot()
                                                                            .getFullPath()));
                            if (file != null && file.exists()) {
                                if (!cleanedFiles.contains(file)) {
                                    cleanedFiles.add(file);
                                    file.deleteMarkers(ViolationErrorMarker.MARKER, true, 1);
                                    file.deleteMarkers(ViolationWarningMarker.MARKER, true, 1);
                                    file.deleteMarkers(InformationMarker.MARKER, true, 1);
                                }
                            }
                            Float limit = Float.valueOf(Float.NaN);
                            boolean violation = false;
                            if (UserPreferencesService.hasMaxValue(check.getId())
                                            && !UserPreferencesService.getMaxValue(check.getId())
                                                            .isNaN()) {
                                limit = UserPreferencesService.getMaxValue(check.getId());
                                violation = check.getValue().compareTo(limit) > 0;
                                if (violation) {
                                    message = getMaximumViolationMessage(check.getName(),
                                                    check.getValue(), limit);
                                } else {
                                    message = getMaximumComplianceMessage(check.getName(),
                                                    check.getValue(), limit);

                                }
                            } else if (UserPreferencesService.hasMinValue(check.getId())
                                            && !UserPreferencesService.getMaxValue(check.getId())
                                                            .isNaN()) {
                                limit = UserPreferencesService.getMinValue(check.getId());
                                violation = check.getValue().compareTo(limit) < 0;
                                if (violation) {
                                    message = getMinimumViolationMessage(check.getName(),
                                                    check.getValue(), limit);
                                } else {
                                    message = getMinimumComplianceMessage(check.getName(),
                                                    check.getValue(), limit);
                                }
                            } else {
                                if (check.getMessage() == null || check.getMessage().isEmpty()) {
                                    if (check.getValue() != null && !check.getValue().isNaN()) {
                                        message = getDefaultMetricComputedMessage(check.getName(),
                                                        check.getValue());
                                    } else {
                                        message = getDefaultMetricUncomputedMessage(
                                                        check.getName());
                                        violation = false;
                                    }
                                } else {
                                    message = check.getName() + " | " + check.getMessage();
                                    violation = true;
                                }

                            }

                            if (violation && UserPreferencesService
                                            .getCheckerSeverity(check.getId())
                                            .equals(UserPreferencesService.PREF_SEVERITY_ERROR_VALUE)) {
                                ViolationErrorMarker.createMarker(file, check.getLine(),
                                                check.getName(), message);
                            } else if (violation && UserPreferencesService
                                            .getCheckerSeverity(check.getId())
                                            .equals(UserPreferencesService.PREF_SEVERITY_WARNING_VALUE)) {
                                ViolationWarningMarker.createMarker(file, check.getLine(),
                                                check.getName(), message);
                            } else {
                                InformationMarker.createMarker(file, check.getLine(),
                                                check.getName(), message);
                            }
                        }
                    } catch (final CoreException exception) {
                        ICodeLogger.error(CLASS, method, exception);
                        MessageDialog.openError(
                                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                                        .getShell(),
                                        "Marker problem", exception.getMessage());

                    }
                }
            });
        } catch (InvocationTargetException | InterruptedException exception) {
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Marker problem", exception.getMessage());
        }
        // One time all markers have been insert, we refresh all
        // decorators.

        final IDecoratorManager manager = PlatformUI.getWorkbench().getDecoratorManager();

        manager.update(ViolationWarningDecorator.ID);
        manager.update(ViolationErrorDecorator.ID_VIOLATION_ERROR_DECORATOR);
        manager.update(InformationDecorator.ID_INFORMATION_DECORATOR);

        ICodeLogger.exiting(CLASS, method);

    }

    /**
     * @param name
     *            of the metric
     * @return Default message when a metric is not being computed for a
     *         function.
     */
    protected static String getDefaultMetricUncomputedMessage(String name) {
        final String method = "getDefaultMetricUncomputedMessage";
        ICodeLogger.entering(CLASS, method, name);
        final String message = name
                        + " | Checker value for this function was not computed. Please refer to CNES"
                        + " RNC for more informations.";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param name
     *            of the metric
     * @param value
     *            of the metric
     * @return default message when a metric is computed.
     */
    protected static String getDefaultMetricComputedMessage(String name, Float value) {
        final String method = "getDefaultMetricComputedMessage";
        ICodeLogger.entering(CLASS, method, new Object[] {
            name, value
        });
        final String message = name + " | Value is " + value + ".";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param name
     *            of the metric
     * @param value
     *            of the metric
     * @param limit
     *            set by the user for the metric
     * @return the error message
     */
    protected static String getMaximumViolationMessage(String name, Float value, Float limit) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, new Object[] {
            name, value, limit
        });
        final String message = name + " | Value is " + value + " while it should not exceed "
                        + limit + ".";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param name
     *            of the metric
     * @param value
     *            of the metric
     * @param limit
     *            set by the user for the metric
     * @return the compliance message
     */
    protected static String getMaximumComplianceMessage(String name, Float value, Float limit) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, new Object[] {
            name, value, limit
        });
        final String message = name + " | Value is " + value + ", below it's maximum limit of "
                        + limit + ".";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param name
     *            of the metric
     * @param value
     *            of the metric
     * @param limit
     *            set by the user for the metric
     * @return the error message
     */
    protected static String getMinimumViolationMessage(String name, Float value, Float limit) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, new Object[] {
            name, value, limit
        });
        final String message = name + " | Value is " + value + " while it should not below " + limit
                        + ".";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param name
     *            of the metric
     * @param value
     *            of the metric
     * @param limit
     *            set by the user for the metric
     * @return the error message
     */
    protected static String getMinimumComplianceMessage(String name, Float value, Float limit) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, new Object[] {
            name, value, limit
        });
        final String message = name + " | Value is " + value + " above it's minimum limit of "
                        + limit + ".";
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

}
