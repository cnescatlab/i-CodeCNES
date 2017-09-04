/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import fr.cnes.analysis.tools.ui.logger.UILogger;

/**
 * AbstractviolationsTreeViewer is an abstract class containing most useful
 * method to create a TreeViewer showing result of a static analysis.
 *
 */
public abstract class AbstractAnalysisTreeViewer extends TreeViewer {

    /** Titles of the columns */
    private String[] titles;

    /** Bounds of the TreeViewer */
    private int[] bounds;

    /** Class name **/
    private final static String CLASS = AbstractAnalysisTreeViewer.class.getName();

    /**
     * @param parent
     *            Composite containing the TreeViewer
     * @param style
     *            Style parameters of the TreeViewer
     * @param pTitles
     *            Columns titles
     * @param pBounds
     *            Bounds of the TreeViewer.
     */
    public AbstractAnalysisTreeViewer(Composite parent, int style, String[] pTitles,
                    int[] pBounds) {
        super(parent, style);

        this.titles = pTitles;
        this.bounds = pBounds;
        this.createColumns();
        this.addDoubleClickAction();
    }

    /**
     * This method must be implemented to define the content provider for the
     * columns of the viewer
     */
    protected abstract void createColumns();

    /**
     * This method must be implemented in order to open a file selected by the
     * user by double clicking one of the TreeViewer content.
     * 
     * {@link #openFileInEditor(IResource, int)}
     */
    protected abstract void addDoubleClickAction();

    /**
     * Method that open a file in the editor when double-clicked, on a specific
     * line.
     * 
     * @param res
     *            the file to open
     * @param line
     *            the line on which the file has to be opened
     */
    protected void openFileInEditor(final IResource res, final int line) {
        final String method = "openFileInEditor";
        UILogger.entering(CLASS, method, new Object[] {
            res, line
        });
        final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage();
        try {
            // Before creating the marker
            res.deleteMarkers(IMarker.MARKER, false, 1);
            final IMarker marker = res.createMarker(IMarker.MARKER);
            marker.setAttribute(IMarker.LINE_NUMBER, line);
            IDE.openEditor(page, marker);
        } catch (final CoreException exception) {
            UILogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Marker problem", exception.getMessage());
        }
        UILogger.exiting(CLASS, method);
    }

    /**
     * @return the titles
     */
    public String[] getTitles() {
        final String method = "getTitles";
        UILogger.entering(CLASS, method);
        UILogger.exiting(CLASS, method, titles);
        return titles;
    }

    /**
     * @param pTitles
     *            the titles to set
     */
    public void setTitles(String[] pTitles) {
        final String method = "setTitles";
        UILogger.entering(CLASS, method, pTitles);

        this.titles = pTitles;
        UILogger.exiting(CLASS, method);
    }

    /**
     * @return the bounds
     */
    public int[] getBounds() {
        final String method = "getBounds";
        UILogger.entering(CLASS, method);
        UILogger.exiting(CLASS, method, bounds);
        return bounds;
    }

    /**
     * @param pBounds
     *            the bounds to set
     */
    public void setBounds(int[] pBounds) {
        final String method = "setBounds";
        UILogger.entering(CLASS, method, pBounds);
        this.bounds = pBounds;
        UILogger.exiting(CLASS, method);
    }
}
