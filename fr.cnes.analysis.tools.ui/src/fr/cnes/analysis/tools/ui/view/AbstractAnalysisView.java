/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import java.util.logging.Logger;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.part.ViewPart;

/**
 * Abstract view defining the basis for violations and metrics results display.
 * 
 */
public abstract class AbstractAnalysisView extends ViewPart {
    /** Logger. **/
    public final static Logger LOGGER = Logger.getLogger(AbstractAnalysisView.class.getName());

    /** Bounds value. **/
    private transient final int[] bounds;
    /** Columns titles. **/
    private transient final String[] titles;

    /** The viewer which display results. **/
    private TreeViewer viewer;

    /**
     * Constructor with an integer array for table bounds and string array for
     * the titles, as parameters.
     * 
     * @param pBounds
     *            viewer columns' bounds
     * @param pTitles
     *            viewer columns' titles
     */
    public AbstractAnalysisView(final int[] pBounds, final String[] pTitles) {
        super();
        this.bounds = pBounds.clone();
        this.titles = pTitles.clone();
    }

    /**
     * Getter for the bounds.
     * 
     * @return the bounds
     */
    public int[] getBounds() {
        return this.bounds.clone();
    }

    /**
     * Getter for the titles.
     * 
     * @return the titles
     */
    public String[] getTitles() {
        return this.titles.clone();
    }

    /**
     * Getter for the viewer.
     * 
     * @return the viewer
     */
    public TreeViewer getViewer() {
        return this.viewer;
    }

    /**
     * Setter for the viewer.
     * 
     * @param pViewer
     *            this.descriptors.clone() set
     */
    public void setViewer(final TreeViewer pViewer) {
        this.viewer = pViewer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.
     * widgets .Composite)
     */
    @Override
    public void createPartControl(final Composite parent) {
        LOGGER.finest("Begin createPartControl method");

        final GridLayout layout = new GridLayout(this.titles.length, false);
        parent.setLayout(layout);
        this.createViewer(parent);

        LOGGER.finest("End createPartControl method");
    }

    /**
     * This method create the viewer, which is a tree table.
     * 
     * @param parent
     *            the parent composite
     */
    private void createViewer(final Composite parent) {
        LOGGER.finest("Begin createViewer method");

        // Defining overall style for TreeViewer
        final int scrollStyle = SWT.H_SCROLL | SWT.V_SCROLL;
        final int selecStyle = SWT.MULTI | SWT.FULL_SELECTION;
        final int style = scrollStyle | selecStyle;
        this.viewer = new TreeViewer(parent, style | SWT.FILL);
        // Make headers and lines of the tree visible
        final Tree tree = this.viewer.getTree();
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);

        // Setting the content provider and creating columns
        this.createColumns();

        // Expand the tree
        this.viewer.setAutoExpandLevel(1);

        // Add selection provider which allows to listen to each
        // selection made on this viewer.
        this.getSite().setSelectionProvider(this.viewer);

        // Add a DoubleClickListener
        this.addDoubleClickAction();

        // TODO: verify XML
        // Fill tree with values from xml
        // this.fillView();

        // Layout the viewer
        final GridData gridData = new GridData();
        gridData.verticalAlignment = GridData.FILL;
        gridData.horizontalSpan = this.titles.length;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        this.viewer.getTree().setLayoutData(gridData);

        LOGGER.finest("End createViewer method");
    }

    /**
     * Action to do when a double click over the item is done
     */
    protected abstract void addDoubleClickAction();

    /**
     * Fill view with the values from the xml file
     */
    protected abstract void fillView();

    /**
     * This method creates all columns of the tree table viewer.
     */
    protected abstract void createColumns();

    /**
     * This method creates a tree viewer column.
     * 
     * @param title
     *            title of the column
     * @param bound
     *            size of the column
     * @return a table viewer's column
     */
    protected TreeViewerColumn createTreeViewerColumn(final String title, final int bound) {
        LOGGER.finest("Begin createTreeViewerColumn method");

        final TreeViewerColumn viewerColumn = new TreeViewerColumn(this.viewer, SWT.NONE);
        final TreeColumn column = viewerColumn.getColumn();
        column.setText(title);
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(false);

        LOGGER.finest("End createTreeViewerColumn method");
        return viewerColumn;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        this.viewer.getControl().setFocus();
    }

}
