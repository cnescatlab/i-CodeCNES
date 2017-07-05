package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule;

import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;

import fr.cnes.analysis.tools.ui.view.AbstractAnalysisTreeViewer;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FunctionRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.RuleDescriptor;

/**
 * ViolationsRuleTreeViewer This class implements an AbstractAnalysisTreeViewer
 * with {@link RuleDescriptor} {@link }
 *
 */
public class RuleTreeViewer extends AbstractAnalysisTreeViewer {

    /** Logger. **/
    public static final Logger LOGGER = Logger.getLogger(RuleTreeViewer.class.getName());

    /** Titles of the columns */
    private static final String[] TITLES = new String[] { " ! ", "Rule", "Line",
            "Number of violations", "Message" };

    /** Bounds of the TreeViewer */
    private static final int[] BOUNDS       = new int[] { 50, 200, 50, 50, 200 };
    /**
     * Kind of bitmap to know if the sorting should be up or down for each
     * column of the tree
     */
    private boolean[]          columnSortUp = new boolean[] { true, true, false, true, true };

    /** Index selected to sort the columns, by default 1 */
    private int indexSort = 1;
    /** Bounds of the columns */

    /**
     * Constructor for violations rule treeviewer.
     * 
     * @param parent
     *            The Composite containing the TreeViewer
     * @param style
     *            The SWT style
     */
    public RuleTreeViewer(Composite parent, int style) {
        super(parent, style, TITLES, BOUNDS);
        final ViewerComparator comparator = new RuleTreeViewerComparator();
        this.setComparator(comparator);

    }

    /**
     * This method creates all columns of the tree table viewer.
     */
    protected void createColumns() {
        LOGGER.finest("Begin createColumns method");

        this.setContentProvider(new RuleTreeViewerContentProvider());
        TreeViewerColumn col;
        for (int i = 0; i < super.getTitles().length; i++) {
            // Create the column
            col = this.createTreeViewerColumn(this.getTitles()[i], this.getBounds()[i], i);
            // Add a label provider
            col.setLabelProvider(new RuleTreeViewerLabelProvider(i));
        }

        LOGGER.finest("End createColumns method");
    }

    /**
     * Create a column of the TreeViewer, customize it and assign it's index
     * number and action listener to be sorted.
     * 
     * @param title
     *            The title of the head column.
     * @param bound
     *            The bound of the column.
     * @param colNumber
     *            The index number of the column in the TreeViewer.
     * @return The treeViewerColumn created.
     */
    private TreeViewerColumn createTreeViewerColumn(final String title, final int bound,
            final int colNumber) {
        final TreeViewerColumn viewerColumn = new TreeViewerColumn(this, SWT.NONE);
        final TreeColumn column = viewerColumn.getColumn();
        column.setText(title);
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(true);

        column.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                columnSortUp[colNumber] = !columnSortUp[colNumber];
                indexSort = colNumber;
                final Tree tree = getTree();
                tree.setSortColumn(column);
                if (columnSortUp[colNumber]) {
                    tree.setSortDirection(SWT.UP);
                } else {
                    tree.setSortDirection(SWT.DOWN);
                }
                refresh();
            }

        });

        return viewerColumn;
    }

    /**
     * Action to do when a double click over the item is done
     */
    protected void addDoubleClickAction() {
        LOGGER.finest("begin method addDoubleClickAction");
        this.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                final TreeViewer tViewer = (TreeViewer) event.getViewer();
                final IStructuredSelection thisSelection = (IStructuredSelection) event
                        .getSelection();
                final Object selectedNode = thisSelection.getFirstElement();

                tViewer.setExpandedState(selectedNode, !tViewer.getExpandedState(selectedNode));

                // if it is a leaf -> open the file
                if (!tViewer.isExpandable(selectedNode)
                        && selectedNode instanceof FunctionRuleDescriptor) {
                    final IPath path = ((FunctionRuleDescriptor) selectedNode).getFilePath();
                    final int number = ((FunctionRuleDescriptor) selectedNode).getValue();
                    // get resource
                    final IFile fileToOpen = ResourcesPlugin.getWorkspace().getRoot()
                            .getFileForLocation(path);
                    final IResource res = fileToOpen;

                    // open file in editor
                    openFileInEditor(res, number);
                }
            }
        });
        LOGGER.finest("end method addDoubleClickAction");
    }

    /**
     * @return the columnSortUp
     */
    public boolean[] getColumnSortUp() {
        return columnSortUp;
    }

    /**
     * @param columnSortUp
     *            the columnSortUp to set
     */
    public void setColumnSortUp(boolean[] columnSortUp) {
        this.columnSortUp = columnSortUp;
    }

    /**
     * This internal class compose the ViewerComparator of the TreeViewer. The
     * compare method is being called everytime a refresh is being called.
     *
     */
    class RuleTreeViewerComparator extends ViewerComparator {

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.
         * viewers.Viewer, java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(Viewer viewer, Object e1, Object e2) {
            int rc = 0;
            if (e1 instanceof RuleDescriptor && e2 instanceof RuleDescriptor) {
                final RuleDescriptor rule1 = (RuleDescriptor) e1;
                final RuleDescriptor rule2 = (RuleDescriptor) e2;

                switch (indexSort) {
                    case 0:
                        rc = rule1.getSeverity().compareTo(rule2.getSeverity());
                        break;
                    case 1:
                        rc = rule1.getName().compareToIgnoreCase(rule2.getName());
                        break;
                    case 3:
                        rc = rule1.getValue() - rule2.getValue();
                        break;
                    default:
                        rc = 0;
                }
            } else if (e1 instanceof FileRuleDescriptor && e2 instanceof FileRuleDescriptor) {
                final FileRuleDescriptor file1 = (FileRuleDescriptor) e1;
                final FileRuleDescriptor file2 = (FileRuleDescriptor) e2;

                switch (indexSort) {
                    case 1:
                        rc = file1.getName().compareToIgnoreCase(file2.getName());
                        break;
                    case 3:
                        rc = file1.getValue() - file2.getValue();
                        break;
                    default:
                        rc = 0;
                }
            } else
                if (e1 instanceof FunctionRuleDescriptor && e2 instanceof FunctionRuleDescriptor) {
                final FunctionRuleDescriptor function1 = (FunctionRuleDescriptor) e1;
                final FunctionRuleDescriptor function2 = (FunctionRuleDescriptor) e2;

                switch (indexSort) {
                    case 1:
                        rc = function1.getLocation().compareToIgnoreCase(function2.getLocation());
                        break;
                    case 2:
                        rc = function1.getValue() - function2.getValue();
                        break;
                    case 4:
                        rc = function1.getName().compareToIgnoreCase(function2.getName());
                        break;
                    default:
                        rc = 0;
                }
            }
            // If descending order, flip the direction
            if (columnSortUp[indexSort]) {
                rc = -rc;
            }
            return rc;
        }
    }

}
