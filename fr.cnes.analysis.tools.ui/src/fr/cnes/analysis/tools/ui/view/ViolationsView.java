/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.ViewPart;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.IUpdatableAnalysisFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewer;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewerContentProvider;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.filter.FileTreeViewerFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewer;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewerContentProvider;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.filter.RuleViewerFilter;

public class ViolationsView extends ViewPart {

    /** Logger. **/
    public final static Logger LOGGER = Logger.getLogger(ViolationsView.class.getName());

    public static final String VIEW_ID = ViolationsView.class.getName();

    public static final String RULE_TREE_VIEWER_TYPE = "RuleTreeViewer";

    /**
     * FILE_TREE_VIEWER_TYPE
     */
    public static final String FILE_TREE_VIEWER_TYPE = "FileTreeViewer";

    /** The project on which was run the displayed analysis */
    private IProject analysisProject;

    /** The user who ran the analysis */
    private String author;

    /** The date of the analysis */
    private String date;

    /** The string to filter results in the TreeViewer */
    private String searchString = "";

    /** Indicate if violation of level warning must be shown */
    private boolean showWarning = true;
    /** Indicate if violation of level error must be shown */
    private boolean showError = true;
    /** Whether or not to show violation of Info severity */
    private boolean showInfo;
    /**
     * Contain the identifier of the type of TreeViewer currently being
     * displayed in the view. By default, the view show a RuleTreeViewer one
     */
    private String treeViewerType = RULE_TREE_VIEWER_TYPE;

    /**
     * The list of all violation used by the TreeViewer of the view, useful to
     * make an export of the view
     */
    private Set<CheckResult> analysisResults = new TreeSet<>(new Comparator<CheckResult>() {

        @Override
        public int compare(final CheckResult value1, final CheckResult value2) {

            int res = value1.getId().compareTo(value2.getId());
            if (res == 0) {
                res = value1.getFile().getAbsoluteFile()
                                .compareTo(value2.getFile().getAbsoluteFile());
            }
            return res;
        }
    });

    /** The viewer which display results. **/
    private TreeViewer viewer;

    /** Composite contained in the view and displaying it's elements */
    private Composite parent;

    /**
     * Constructor with an integer array for table bounds and string array for
     * the titles, as parameters.
     */
    public ViolationsView() {
        super();
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
     * widgets.Composite)
     */
    @Override
    public void createPartControl(final Composite pParent) {
        LOGGER.finest("Begin createPartControl method");
        this.parent = pParent;
        final GridLayout layout = new GridLayout();

        pParent.setLayout(layout);
        /*
         * Adding the filter field
         */
        final Text search = new Text(pParent, SWT.SEARCH | SWT.CANCEL | SWT.ICON_SEARCH);
        search.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
        search.setMessage("Filter : Enter part of file path, name, rule name or function's name... "
                        + "(not case sensitive)");

        /*
         * Updating search attribute every time the search field is being
         * modified.
         */
        search.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent event) {
                final Text source = (Text) event.getSource();
                searchString = source.getText();
                update();
            }

        });

        /*
         * Add a selection adapter for the button SWT.CANCEL of the search field
         * that set the searching field to null when the user click on the
         * button. Note : SWT.CANCEL do not exist in Windows 7.
         */
        search.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetDefaultSelected(final SelectionEvent event) {
                if (event.detail == SWT.CANCEL) {
                    final Text text = (Text) event.getSource();
                    text.setText("");
                    update();
                }
            }
        });

        final Button infoBtn = new Button(pParent, SWT.CHECK | SWT.SELECTED);
        infoBtn.setText("Info");
        infoBtn.setSelection(true);
        final Button warningBtn = new Button(pParent, SWT.CHECK | SWT.SELECTED);
        warningBtn.setText("Warning");
        warningBtn.setSelection(true);
        final Button errorBtn = new Button(pParent, SWT.CHECK | SWT.CHECK);
        errorBtn.setText("Error");
        errorBtn.setSelection(true);

        infoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                final Button btn = (Button) event.getSource();
                showInfo = btn.getSelection();
                update();
            }

        });

        warningBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent event) {
                final Button btn = (Button) event.getSource();
                showWarning = btn.getSelection();
                update();
            }

        });

        errorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                final Button btn = (Button) event.getSource();
                showError = btn.getSelection();
                update();
            }
        });

        this.createRuleTreeViewer(pParent);
        layout.numColumns = this.getViewer().getTree().getColumnCount();
        LOGGER.finest("End createPartControl method");
    }

    /**
     * 
     */
    public void update() {
        for (final ViewerFilter filter : this.viewer.getFilters()) {
            if (filter instanceof IUpdatableAnalysisFilter) {
                ((IUpdatableAnalysisFilter) filter).update(searchString, showInfo, showWarning,
                                showError);
            }
        }
        viewer.refresh();
    }

    /**
     * This method create the viewer, which is a tree table.
     * 
     * @param parent
     *            the parent composite
     */
    private void createRuleTreeViewer(final Composite parent) {
        LOGGER.finest("Begin createViewer method");

        // Defining overall style for TreeViewer
        final int scrollStyle = SWT.H_SCROLL | SWT.V_SCROLL;
        final int selecStyle = SWT.MULTI | SWT.FULL_SELECTION;
        final int style = scrollStyle | selecStyle;
        this.viewer = new RuleTreeViewer(parent, style | SWT.FILL);
        // Make headers and lines of the tree visible
        final Tree tree = this.viewer.getTree();
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);

        // Expand the tree
        this.viewer.setAutoExpandLevel(1);

        // Add selection provider which allows to listen to each
        // selection made on this viewer.
        this.getSite().setSelectionProvider(this.viewer);

        // TODO: verify XML
        // Fill tree with values from xml
        // this.fillView();

        // Layout the viewer
        final GridData gridData = new GridData();
        gridData.verticalAlignment = GridData.FILL;
        gridData.horizontalSpan = this.viewer.getTree().getColumnCount();
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        this.viewer.getTree().setLayoutData(gridData);

        /*
         * Creating a filter using field search & check buttons Warning and
         * Error.
         * 
         * Show only element selected by the user.
         */
        final RuleViewerFilter ruleFilter = new RuleViewerFilter();
        viewer.addFilter(ruleFilter);

        LOGGER.finest("End createViewer method");

    }

    /**
     * This method create the viewer, which is a tree table.
     * 
     * @param pParent
     *            the parent composite
     */
    private void createFileTreeViewer(final Composite pParent) {
        LOGGER.finest("Begin createViewer method");

        // Defining overall style for TreeViewer
        final int scrollStyle = SWT.H_SCROLL | SWT.V_SCROLL;
        final int selecStyle = SWT.MULTI | SWT.FULL_SELECTION;
        final int style = scrollStyle | selecStyle;
        this.viewer = new FileTreeViewer(pParent, style | SWT.FILL);
        // Make headers and lines of the tree visible
        final Tree tree = this.viewer.getTree();
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);

        // Expand the tree
        this.viewer.setAutoExpandLevel(1);

        // Add selection provider which allows to listen to each
        // selection made on this viewer.
        this.getSite().setSelectionProvider(this.viewer);

        // TODO: verify XML
        // Fill tree with values from xml
        // this.fillView();

        // Layout the viewer
        final GridData gridData = new GridData();
        gridData.verticalAlignment = GridData.FILL;
        gridData.horizontalSpan = this.viewer.getTree().getColumnCount();
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        this.viewer.getTree().setLayoutData(gridData);

        /*
         * Creating a filter using field search & check buttons Warning and
         * Error.
         * 
         * Show only element selected by the user.
         */
        final FileTreeViewerFilter fileFilter = new FileTreeViewerFilter();
        this.viewer.addFilter(fileFilter);

        LOGGER.finest("End createViewer method");

    }

    /**
     * Fill view with the values from the xml file
     */
    protected void fillView() {
        // TODO Fille view with XML file
    }

    /**
     * Display violations found during analysis in the violations view.
     * 
     * @param violations
     *            the violations to display
     * @param pDate
     *            The date of the analysis
     * @param pAuthor
     *            The user who ran the analysis
     * @param pProject
     *            The project selected while running the analysis
     */
    public void display(final List<CheckResult> violations) {
        LOGGER.finest("Begin display(Descriptor[]) method");

        synchronized (this) {
            final Set<CheckResult> listInputs = new TreeSet<CheckResult>(
                            new Comparator<CheckResult>() {

                                @Override
                                public int compare(final CheckResult check1,
                                                final CheckResult check2) {
                                    int res = check1.getName().split("\\.")[0]
                                                    .compareTo(check2.getName().split("\\.")[0]);
                                    if (res == 0) {
                                        res = check1.getName().split("\\.")[1].compareTo(
                                                        check2.getName().split("\\.")[1]);
                                        if (res == 0) {
                                            res = check1.getName().split("\\.")[2].compareTo(
                                                            check2.getName().split("\\.")[2]);
                                            if (res == 0) {
                                                res = check1.getFile().getAbsolutePath().compareTo(
                                                                check2.getFile().getAbsolutePath());
                                                if (res == 0) {
                                                    res = check1.getLine()
                                                                    .compareTo(check2.getLine());
                                                    if (res == 0) {
                                                        res = check1.getLocation().compareTo(
                                                                        check2.getLocation());
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    return res;
                                }

                            });

            if (this.treeViewerType.equals(FILE_TREE_VIEWER_TYPE)
                            && ((FileTreeViewerContentProvider) this.viewer.getContentProvider())
                                            .getConverter().getInputs() != null) {
                for (final CheckResult value : ((FileTreeViewerContentProvider) this.getViewer()
                                .getContentProvider()).getConverter().getInputs()) {
                    listInputs.add(value);
                }
            }
            if (this.treeViewerType.equals(RULE_TREE_VIEWER_TYPE)
                            && ((RuleTreeViewerContentProvider) this.viewer.getContentProvider())
                                            .getConverter().getInputs() != null) {
                for (final CheckResult value : ((RuleTreeViewerContentProvider) this.getViewer()
                                .getContentProvider()).getConverter().getInputs()) {
                    listInputs.add(value);
                }
            }
            for (final CheckResult value : violations) {
                listInputs.add(value);
            }

            this.analysisResults = listInputs;

            this.getViewer().setInput(listInputs.toArray(new CheckResult[listInputs.size()]));
        }
        this.getViewer().refresh();

        LOGGER.finest("End display(Descriptor[]) method");
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

    /**
     * This method will clear the message and make it appear on the view.
     * 
     * @throws EmptyProviderException
     *             when source provider to determine view type is not found (not
     *             necessarily used)
     */
    public void clear() throws EmptyProviderException {
        this.analysisResults.clear();
        this.getViewer().setInput(new CheckResult[0]);
        this.getViewer().refresh();
    }

    /**
     * @return the Treeviewer type
     */
    public String getTreeViewerType() {
        return this.treeViewerType;
    }

    /**
     * Set the TreeViewerType by modifying the attribute and also processing to
     * dispose if necessary the old TreeViewer and create a new one with the
     * type requested.
     * 
     * @param name
     *            Name or identifier of the TreeViewerType requested.
     */
    public void setTreeViewerType(final String name) {
        if (!this.treeViewerType.equals(name)) {
            // Disposal of the old TreeViewer
            this.viewer.getControl().dispose();
            if (name.equals(FILE_TREE_VIEWER_TYPE)) {
                this.createFileTreeViewer(this.parent);
                // We reinsert inputs from previous TreeViewer in the current
                // one
                this.getViewer().setInput(this.analysisResults
                                .toArray(new CheckResult[this.analysisResults.size()]));
                this.treeViewerType = name;

            } else if (name.equals(RULE_TREE_VIEWER_TYPE)) {
                this.createRuleTreeViewer(this.parent);
                // We reinsert inputs from previous TreeViewer in the current
                // one
                this.getViewer().setInput(this.analysisResults
                                .toArray(new CheckResult[this.analysisResults.size()]));
                this.treeViewerType = name;

            }
            // This call is necessary to refresh the table in the parent
            // Composite.
            this.parent.layout();
        }

    }

    public Set<CheckResult> getAnalysisResults() {
        return analysisResults;
    }

    public void setAnalysisResults(Set<CheckResult> analysisResults) {
        this.analysisResults = analysisResults;
    }

}
