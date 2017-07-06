package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file;

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
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FunctionDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.RuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.ViolationDescriptor;

/**
 * TreeViewer that show violations in {@link #getInput()} organized by their :
 * <ul>
 * <li>{@link FileRuleDescriptor}</li>
 * <li>{@link FunctionDescriptor}</li>
 * <li>{@link RuleDescriptor}</li>
 * <li>All the {@link ViolationDescriptor} are contained inside
 * {@link RuleDescriptor}</li>
 * </ul>
 * 
 * It also contains information as the number of violations on each files,
 * functions or rules and can be ordered and filtered, using a
 * {@link FileTreeViewerComparator}. </br>
 * 
 * @see fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewer
 * 
 * @version 2.1
 * @since 2.0
 */
public class FileTreeViewer extends AbstractAnalysisTreeViewer {

    /**
     * TITLES : Location => File > Function > Rules > Violations Line => Line of
     * the violation ! => Criticity Number of violation.
     */
    public static final String[] TITLES = new String[] { "Location", "Line", "!",
            "Number of violations" };

    /** Bounds of the TreeViewer */
    public static final int[] BOUNDS = new int[] { 425, 50, 30, 200 };
    /**
     * Kind of bitmap to know if the sorting should be up or down for each
     * column of the tree
     */
    private transient boolean[] columnSortUp = new boolean[] { true, true, false, true, true };

    /** Index selected to sort the columns, by default 1 */
    private transient int indexSort = 1;

    /**
     * Constructor for violations file treeviewer.
     * 
     * @param parent
     *            The Composite containing the TreeViewer
     * @param style
     *            The SWT style
     */
    public FileTreeViewer(Composite parent, int style) {
        super(parent, style, TITLES, BOUNDS);
        final ViewerComparator comparator = new FileTreeViewerComparator();
        this.setComparator(comparator);
    }

    @Override
    protected void createColumns() {
        this.setContentProvider(new FileTreeViewerContentProvider());
        TreeViewerColumn col;
        for (int i = 0; i < super.getTitles().length; i++) {
            // Create the column
            col = this.createTreeViewerColumn(this.getTitles()[i], this.getBounds()[i], i);
            // Add a label provider
            col.setLabelProvider(new FileTreeViewerLabelProvider(i));
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisTreeViewer#
     * addDoubleClickAction()
     */
    @Override
    protected void addDoubleClickAction() {
        LOGGER.finest("begin method addDoubleClickAction");
        this.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                final TreeViewer tViewer = (TreeViewer) event.getViewer();
                final IStructuredSelection thisSelection = (IStructuredSelection) event
                        .getSelection();
                final Object selectedNode = thisSelection.getFirstElement();

                // if it is a leaf -> open the file
                if (!tViewer.isExpandable(selectedNode)
                        && selectedNode instanceof ViolationDescriptor) {
                    final IPath path = ((ViolationDescriptor) selectedNode).getFilePath();
                    final int number = ((ViolationDescriptor) selectedNode).getValue();
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

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.
             * eclipse.swt.events.SelectionEvent)
             */
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
     * Comparator for class {@link FileTreeViewer}. Allow the user to order the
     * file using different column sort.
     *
     */
    class FileTreeViewerComparator extends ViewerComparator {

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
                    case 2:
                        rc = rule1.getSeverity().compareTo(rule2.getSeverity());
                        break;
                    case 0:
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
                    case 0:
                        rc = file1.getName().compareToIgnoreCase(file2.getName());
                        break;
                    case 3:
                        rc = file1.getValue() - file2.getValue();
                        break;
                    default:
                        rc = 0;
                }
            } else if (e1 instanceof FunctionDescriptor && e2 instanceof FunctionDescriptor) {

                final FunctionDescriptor function1 = (FunctionDescriptor) e1;
                final FunctionDescriptor function2 = (FunctionDescriptor) e2;

                switch (indexSort) {
                    case 0:
                        rc = function1.getName().compareToIgnoreCase(function2.getName());
                        break;
                    case 3:
                        rc = function1.getValue() - function2.getValue();
                        break;
                    default:
                        rc = 0;
                }
            } else if (e1 instanceof ViolationDescriptor && e2 instanceof ViolationDescriptor) {
                final ViolationDescriptor violation1 = (ViolationDescriptor) e1;
                final ViolationDescriptor violation2 = (ViolationDescriptor) e2;

                switch (indexSort) {
                    case 0:
                        rc = violation1.getLocation().compareToIgnoreCase(violation2.getLocation());
                        break;
                    case 1:
                        rc = violation1.getValue() - violation2.getValue();
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
