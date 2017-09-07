/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;

/**
 * Handler to clear the views.
 * 
 */
public class ClearHandler extends AbstractHandler {

    /** Class name */
    private static final String CLASS = ClearHandler.class.getName();

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.
     * commands .ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent event) {
        final String method = "execute";
        ICodeLogger.entering(this.getClass().getName(), method, event);
        try {

            // clear the violations view
            this.clearViolationsView();

            // clear the metrics view
            this.clearMetricsView();

            // delete markers
            this.clearAllMarkers();

            this.refreshDecorators();

        } catch (final EmptyProviderException exception) {
            ICodeLogger.error(this.getClass().getName(), method, exception);
            MessageDialog.openError(HandlerUtil.getActiveShell(event), "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(this.getClass().getName(), method, null);
        return null;
    }

    /**
     * Simply refresh all decorators to remove them from the IResource of type
     * IFile in the files tree.
     * 
     */
    private void refreshDecorators() {
        final String method = "refreshDecorators";
        ICodeLogger.entering(CLASS, method);
        final IDecoratorManager manager = PlatformUI.getWorkbench().getDecoratorManager();

        manager.update("fr.cnes.analysis.tools.ui.decorators.violationwarningdecorator");
        manager.update("fr.cnes.analysis.tools.ui.decorators.violationerrordecorator");

        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * This method clear the violation view if it is opened.
     * 
     * @throws EmptyProviderException
     *             error throw whenever the provider corresponding to the view
     *             can't be found
     */
    private void clearViolationsView() throws EmptyProviderException {
        final String method = "clearViolationsView";
        ICodeLogger.entering(CLASS, method);

        final ViolationsView rulesView = (ViolationsView) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .findView(ViolationsView.VIEW_ID);
        if (rulesView != null) {
            rulesView.clear();
        }
        ICodeLogger.exiting(CLASS, method);

    }

    /**
     * This method clear the metric view if it is opened.
     * 
     * @throws EmptyProviderException
     *             error throw whenever the provider corresponding to the view
     *             can't be found
     */
    private void clearMetricsView() throws EmptyProviderException {
        final String method = "clearMetricsView";
        ICodeLogger.entering(CLASS, method);

        final MetricsView metricsView = (MetricsView) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage().findView(MetricsView.VIEW_ID);
        if (metricsView != null) {
            metricsView.clear();
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * This method clear all the markers errors and warning from files.
     */
    private void clearAllMarkers() {
        final String method = "clearAllMarkers";
        ICodeLogger.entering(CLASS, method);

        final IResource resource = ResourcesPlugin.getWorkspace().getRoot();
        final int depth = IResource.DEPTH_INFINITE;
        try {
            resource.deleteMarkers("fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker", true,
                            depth);
            resource.deleteMarkers("fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker", true,
                            depth);
            resource.deleteMarkers("fr.cnes.analysis.tools.ui.markers.InformationMarker", true,
                            depth);
        } catch (final CoreException exception) {
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Marker deletion problem", exception.getMessage());
        }
        ICodeLogger.exiting(CLASS, method);
    }
}
