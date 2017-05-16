/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.editors;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewerContentProvider;

// NOTE : 08/02/2016 - This class is not used anymore

/**
 * Part listener for violation's markers on files in the editor.
 */
public class ViolationPartListener implements IPartListener2 {

    private final static Logger LOGGER = Logger.getLogger(MetricsView.class.getName());

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partActivated(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partActivated(final IWorkbenchPartReference partRef) {
        final IPath filePath = this.getEditorFile(partRef);
        if (filePath != null) {
            try {
                ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(filePath).deleteMarkers(
                        "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker", true, 1);

                ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(filePath).deleteMarkers(
                        "fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker", true, 1);
            } catch (CoreException exception) {
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Marker problem", exception.getMessage());
            }
            this.addMarkers(filePath);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partBroughtToTop(org.eclipse.ui
     * .IWorkbenchPartReference)
     */
    @Override
    public void partBroughtToTop(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partClosed(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partDeactivated(org.eclipse.ui
     * .IWorkbenchPartReference)
     */
    @Override
    public void partDeactivated(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partOpened(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partOpened(final IWorkbenchPartReference partRef) {
        final IPath file = this.getEditorFile(partRef);
        if (file != null) {
            this.addMarkers(file);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partHidden(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partHidden(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partVisible(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partInputChanged(org.eclipse.ui
     * .IWorkbenchPartReference)
     */
    @Override
    public void partInputChanged(final IWorkbenchPartReference partRef) {
        // do nothing
    }

    /**
     * Returns the file behind the referenced workbench part.
     * 
     * @param partRef
     *            the workbench part in question
     * @return the editors file or null if the workbench part is no file based
     *         editor
     */
    private IPath getEditorFile(final IWorkbenchPartReference partRef) {
        IPath file = null;
        if ((partRef instanceof IEditorReference)) {
            IEditorInput input = null;
            if (partRef.getPart(false) instanceof IEditorPart) {
                input = ((IEditorPart) partRef.getPart(false)).getEditorInput();
            }
            if (input instanceof FileEditorInput) {
                file = ((FileEditorInput) input).getPath();
            }
        }
        return file;
    }

    /**
     * Add markers that belongs to the file.
     * 
     * @param path
     *            the file's path
     */
    private void addMarkers(final IPath path) {
        this.insertMarkersInResource(path,
                ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(path));
    }

    /**
     * This function insert the markers (Problem markers) in the corresponding
     * resource. The error is getting from the Analyzer Manager.
     * 
     * @param filePath
     *            file's path
     * @param res
     *            corresponding resource
     */
    public void insertMarkersInResource(final IPath filePath, final IResource res) {

        if (this.getViolations() != null) {
            for (final Violation violation : this.getViolations()) {
                if (violation.getFile().equals(filePath)) {
                    try {
                        // Get cirticity from preferences to insert the correct
                        // icon
                        final String criticity = PlatformUI.getPreferenceStore()
                                .getString(violation.getRuleId() + ".Criticity");
                        // Create marker
                        final IMarker marker;
                        if (criticity.equals("Error")) {
                            marker = res.createMarker(
                                    "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker");
                        } else if (criticity.equals("Warning")) {
                            marker = res.createMarker(
                                    "fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker");
                        } else {
                            marker = res.createMarker(IMarker.PROBLEM);
                        }
                        marker.setAttribute(IMarker.LINE_NUMBER, violation.getLine());
                        marker.setAttribute(IMarker.MESSAGE, violation.getRuleName());
                        marker.setAttribute("Description", violation.getRuleName());
                        marker.setAttribute("Class", "Violation");
                    } catch (final CoreException exception) {
                        MessageDialog.openError(
                                PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                                "Marker problem", exception.getMessage());
                    }
                }
            }
        }
    }

    /**
     * Method to get all violations of the analysis made before. If the view or
     * its content provider are null, violations returned is null. Violations'
     * tab can also be null.
     * 
     * @return violations appeared during analysis, or null
     */
    private Violation[] getViolations() {
        Violation[] violations = null;
        final ViolationsView view = (ViolationsView) PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().findView(ViolationsView.VIEW_ID);
        if (view != null) {
            final RuleTreeViewerContentProvider contentProv = (RuleTreeViewerContentProvider) view.getViewer()
                    .getContentProvider();
            if (contentProv != null) {
                violations = contentProv.getConverter().getInputs();
            }
        }
        return violations;
    }
}
