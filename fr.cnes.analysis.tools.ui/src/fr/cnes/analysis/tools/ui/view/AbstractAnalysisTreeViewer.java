/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * AbstractviolationsTreeViewer is an abstract class containing most useful
 * method to create a TreeViewer showing result of a static analysis.
 *
 */
public abstract class AbstractAnalysisTreeViewer extends TreeViewer {

    public final static Logger LOGGER = Logger
                    .getLogger(AbstractAnalysisTreeViewer.class.getName());

    /** Titles of the columns */
    private String[] titles;

    /** Bounds of the TreeViewer */
    private int[] bounds;

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
        LOGGER.finest("begin method openFileInEditor");
        final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage();
        try {
            // Before creating the marker
            res.deleteMarkers(IMarker.MARKER, false, 1);
            final IMarker marker = res.createMarker(IMarker.MARKER);
            marker.setAttribute(IMarker.LINE_NUMBER, line);
            IDE.openEditor(page, marker);
        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Marker problem", exception.getMessage());
        }
        LOGGER.finest("end method openFileInEditor");
    }

    /**
     * @return the titles
     */
    public String[] getTitles() {
        return titles;
    }

    /**
     * @param pTitles
     *            the titles to set
     */
    public void setTitles(String[] pTitles) {
        this.titles = pTitles;
    }

    /**
     * @return the bounds
     */
    public int[] getBounds() {
        return bounds;
    }

    /**
     * @param pBounds
     *            the bounds to set
     */
    public void setBounds(int[] pBounds) {
        this.bounds = pBounds;
    }
}
