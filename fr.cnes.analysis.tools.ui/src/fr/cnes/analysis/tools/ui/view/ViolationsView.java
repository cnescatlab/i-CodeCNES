package fr.cnes.analysis.tools.ui.view;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.IUpdatableAnalysisFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewer;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewerContentProvider;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.filter.FileTreeViewerFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewer;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewerContentProvider;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.filter.RuleViewerFilter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
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
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.part.ViewPart;
import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;
import org.xml.sax.SAXException;

public class ViolationsView extends ViewPart implements IExportableView {

    /** Logger. **/
    public final static Logger LOGGER = Logger.getLogger(ViolationsView.class.getName());

    public static final String VIEW_ID = ViolationsView.class.getName();

    public static final String RULE_TREE_VIEWER_TYPE = "RuleTreeViewer";

    /**
     * FILE_TREE_VIEWER_TYPE
     */
    public static final String FILE_TREE_VIEWER_TYPE = "FileTreeViewer";

    /** The project on which was run the displayed analysis */
    private String analysisProject = "undefined";

    /** The user who ran the analysis */
    private String author = "undefined";

    /** The date of the analysis */
    private String date = "undefined";

    /** The string to filter results in the TreeViewer */
    private String searchString = "";

    /** Indicate is violation of level warning must be shown */
    private boolean showWarning = true;
    /** Indicate is violation of level error must be shown */
    private boolean showError = true;

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

        final Button warningBtn = new Button(pParent, SWT.CHECK | SWT.SELECTED);
        warningBtn.setText("Warning");
        warningBtn.setSelection(true);
        final Button errorBtn = new Button(pParent, SWT.CHECK | SWT.CHECK);
        errorBtn.setText("Error");
        errorBtn.setSelection(true);
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
                ((IUpdatableAnalysisFilter) filter).update(searchString, showWarning, showError);
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
     * @param pCheckResults
     *            the violations to display
     * @param pDate
     *            The date of the analysis
     * @param pAuthor
     *            The user who ran the analysis
     * @param pProject
     *            The project selected while running the analysis
     */
    public void display(final List<CheckResult> pCheckResults) {
        LOGGER.finest("Begin display(Descriptor[]) method");

        synchronized (this) {
            final Set<CheckResult> listInputs = new TreeSet<CheckResult>(
                    new Comparator<CheckResult>() {

                        @Override
                        public int compare(final CheckResult result1, final CheckResult result2) {
                            int res = result1.getName().split("\\.")[0]
                                    .compareTo(result2.getName().split("\\.")[0]);
                            if (res == 0) {
                                res = result1.getName().split("\\.")[1]
                                        .compareTo(result2.getName().split("\\.")[1]);
                                if (res == 0) {
                                    res = result1.getName().split("\\.")[2]
                                            .compareTo(result2.getName().split("\\.")[2]);
                                    if (res == 0) {
                                        res = result1.getFile().getAbsolutePath()
                                                .compareTo(result2.getFile().getAbsolutePath());
                                        if (res == 0) {
                                            res = result1.getLine().compareTo(result2.getLine());
                                            if (res == 0) {
                                                res = result1.getLocation()
                                                        .compareTo(result2.getLocation());
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
            for (final CheckResult value : pCheckResults) {
                listInputs.add(value);
            }

            this.analysisResults = listInputs;

            this.getViewer().setInput(listInputs.toArray(new CheckResult[listInputs.size()]));
        }
        this.getViewer().refresh();
        try {
            this.insertMarkers();
        } catch (InvocationTargetException exception) {
            LOGGER.log(Level.WARNING, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (InterruptedException exception) {
            LOGGER.log(Level.WARNING, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End display(Descriptor[]) method");
    }

    /**
     * This method insert for each violation detected a new marker on the line
     * of the violation.
     * 
     * @throws InterruptedException
     * @throws InvocationTargetException
     */
    public void insertMarkers() throws InvocationTargetException, InterruptedException {
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
                    String ruleName, criticity, message = "Violation detected here.";
                    Integer line;
                    IFile file;
                    for (final CheckResult checkResult : analysisResults) {
                        if (checkResult.getMessage().isEmpty()) {
                            message = "No message in description. Please refer to CNES RNC.";
                        } else {
                            message = checkResult.getMessage();
                        }
                        ruleName = checkResult.getName();
                        criticity = PlatformUI.getPreferenceStore()
                                .getString(checkResult.getId() + ".Criticity");
                        line = checkResult.getLine();
                        file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(
                                new Path(checkResult.getFile().getAbsolutePath()).makeRelativeTo(
                                        ResourcesPlugin.getWorkspace().getRoot().getFullPath()));

                        // If the file already has marker of type violations
                        // then we clean the file once
                        if (file != null && !cleanedFiles.contains(file)) {
                            cleanedFiles.add(file);
                            file.deleteMarkers(
                                    "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker", true,
                                    1);
                            file.deleteMarkers(
                                    "fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker",
                                    true, 1);
                        }
                        // Then we add the new markers
                        if (file != null && file.exists()) {
                            if ("Error".equals(criticity)) {

                                // If the criticity is at level "Error" then we
                                // pop
                                // a
                                // new ViolationErrorMarker.
                                ViolationErrorMarker.createMarker(file, line, ruleName,
                                        ruleName + " | " + message);

                            } else if ("Warning".equals(criticity)) {
                                // else if the criticity is set on Warning then
                                // we
                                // pop a ViolationWarningMarker
                                // new ViolationErrorDecorator();
                                ViolationWarningMarker.createMarker(file, line, ruleName,
                                        ruleName + " | " + message);

                            } else {
                                // Last case shouldn't happen if the criticity
                                // is
                                // properly set however we had a
                                // PROBLEM marker on the line where a rule
                                // spotted a
                                // problem.
                                IMarker markerProblem;
                                markerProblem = file.createMarker(IMarker.PROBLEM);
                                markerProblem.setAttribute(IMarker.LINE_NUMBER, line);
                                markerProblem.setAttribute(IMarker.MESSAGE,
                                        ruleName + " | " + message);
                                markerProblem.setAttribute("Description", ruleName);
                            }
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

        LOGGER.finest("end method insertMarkers");

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.IExportableView#exportToCSV(java.io.File)
     */
    @Override
    public void exportToCSV(final File file) throws IOException {
        final FileWriter out = new FileWriter(file);
        out.write("Rule, File, Location, Value, Criticity\n");
        for (final CheckResult checkResult : (CheckResult[]) this.getViewer().getInput()) {
            out.write(checkResult.getName() + "," + checkResult.getFile().getAbsolutePath() + ","
                    + checkResult.getLocation() + "," + checkResult.getLine().toString() + "\n");
        }

        out.close();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.IExportableView#exportToXML(java.io.File)
     */
    @Override
    public void exportToXML(final File file) throws IOException {
        LOGGER.finest("begin method toXML");
        final List<Attribute> attributes = new ArrayList<Attribute>();
        final List<Attribute> resultAttributes = new ArrayList<Attribute>();
        /*
         * Creation of the root element <xsd:element name="analysisProject"
         * type="anr:analysisProjectType" minOccurs="1" maxOccurs="1" />
         */

        final Element analysisProjectElement = new Element("analysisProject");
        analysisProjectElement.setAttribute(new Attribute("analysisProjectName", analysisProject));
        final Document document = new Document(analysisProjectElement);

        // BEGINNING OF SEQUENCE <xsd:sequence>
        // -- <xsd:element name="analysisInformations"
        // -- type="anr:analysisInformationsType" minOccurs="1"
        // maxOccurs="1">

        final Element analysisInformation = new Element("analysisInformations");
        attributes.add(new Attribute("analysisConfigurationId", "standard"));
        attributes.add(new Attribute("analysisDate", this.date));
        attributes.add(new Attribute("author", this.author));

        analysisInformation.setAttributes(attributes);
        document.getRootElement().addContent(analysisInformation);
        // -- <xsd:element name="analysisFile" type="anr:analysisFileType"
        // -- minOccurs="0" maxOccurs="unbounded" />
        for (final CheckResult violation : this.analysisResults) {
            attributes.clear();

            // -- <xsd:attribute name="fileName" type="xsd:string"
            // -- use="required" />
            // Note : We take only the last segment of the filepath to
            // get
            // the filename.
            // -- <xsd:attribute name="language" type="xsd:string" />
            final String language = this.getFileExtension(file.getAbsolutePath());
            final String fileName = violation.getFile().getAbsolutePath();
            // The analysisFile element is being added only and only if it's not
            // already in the XML document.
            boolean analysisFileMarked = false;
            for (final Element element : document.getRootElement().getChildren("analysisFile")) {
                if (element.getAttributeValue("fileName").equals(fileName)
                        && element.getAttributeValue("language").equals(language)) {
                    analysisFileMarked = true;
                }
            }
            if (!analysisFileMarked) {
                final Element analysisFile = new Element("analysisFile");
                attributes.add(new Attribute("language", language));
                attributes.add(new Attribute("fileName", fileName));
                analysisFile.setAttributes(attributes);
                document.getRootElement().addContent(analysisFile);
            }
        }
        Element result, analysisRule = null;
        int resultId = 1;
        for (final CheckResult violation : this.analysisResults) {
            boolean elementAlreadyExisting = false;
            Element existingElement = null;
            for (final Element element : document.getRootElement().getChildren("analysisRule")) {
                for (final Attribute attribute : element.getAttributes()) {
                    if (attribute.getName().equals("analysisRuleId")
                            && attribute.getValue().equals(violation.getId())) {
                        elementAlreadyExisting = true;
                        existingElement = element;
                    }
                }
            }
            if (!elementAlreadyExisting || existingElement == null) {
                analysisRule = new Element("analysisRule");
                analysisRule.setAttribute(new Attribute("analysisRuleId", violation.getName()));
            } else {
                analysisRule = existingElement;
            }

            result = new Element("result");
            resultAttributes.clear();
            // no justification for now
            // no outputDetail for now neither

            resultAttributes.add(new Attribute("resultId", Integer.toString(resultId)));
            resultId++;
            resultAttributes.add(new Attribute("fileName", violation.getFile().getAbsolutePath()));
            resultAttributes.add(new Attribute("resultLine", violation.getLine().toString()));

            /*
             * The location and message are defined in violations by only one
             * attribute (String) made this way: [Location -> Message], so we
             * split it to get the two exploitable strings.
             */
            resultAttributes.add(new Attribute("resultNamePlace", violation.getLocation()));
            /*
             * The result message is defined by the XSD as a sequence of element
             * resultMessage (not an attribute).
             */
            final Element resultMessage = new Element("resultMessage");

            resultMessage.addContent(violation.getMessage());

            result.addContent(resultMessage);
            result.setAttributes(resultAttributes);
            /*
             * The result is being added to the analysisRule element;
             */
            analysisRule.addContent(result);

            /*
             * If the rule analysisRule was already in the document, then it's
             * not necessary to add it again in the document, however if it's
             * the first time that this analysisRule appear in the document we
             * have to add it to the Root element.
             */
            if (!elementAlreadyExisting) {
                document.getRootElement().addContent(analysisRule);
            }
        }

        final XMLOutputter xmlOutput = new XMLOutputter();
        xmlOutput.setFormat(Format.getPrettyFormat());
        final FileOutputStream fileOutput = new FileOutputStream(file);

        xmlOutput.output(document, fileOutput);
        fileOutput.close();
        this.verifyXMLandXSDValidity(file);

        LOGGER.finest("end method toXML");

    }

    /**
     * @param fileName
     * @return The extension name of the file
     */
    private String getFileExtension(String fileName) {
        String extension = "unknown";

        int i = fileName.lastIndexOf('.');
        int p = Math.max(fileName.lastIndexOf('/'), fileName.lastIndexOf('\\'));

        if (i > p) {
            extension = fileName.substring(i + 1);
        }
        return extension;
    }

    /**
     * @param file
     *            The XML file to validate
     * 
     * @return Returns if XSD is W3C valid and XML is W3C valid and respecting
     *         XSD specifications.
     * 
     * @throws IOException
     *             when XSD file can't be read.
     */
    public boolean verifyXMLandXSDValidity(final File file) throws IOException {
        LOGGER.finest("begin method verifyXMLandXSDValidity");
        boolean validity;
        try {
            final SchemaFactory factory = SchemaFactory
                    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

            // TODO : Find better way to locate the XSD file in resources
            final URL globalPath = new URL(
                    this.getClass().getProtectionDomain().getCodeSource().getLocation()
                            + MetricsView.XSD_FILEPATH);

            final Schema dda = factory.newSchema(globalPath);

            final Validator validator = dda.newValidator();
            validator.validate(new StreamSource(file));
            validity = true;
        } catch (SAXException exception) {
            validity = false;
            exception.printStackTrace();
        }
        LOGGER.finest("end method verifyXMLandXSDValidity");
        return validity;

    }

    /**
     * This method will clear the message and make it appear on the view.
     * 
     * @throws EmptyProviderException
     *             when source provider to determine view type is not found (not
     *             necessarily used)
     */
    public void clear() throws EmptyProviderException {
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
                this.getViewer().setInput(
                        this.analysisResults.toArray(new CheckResult[this.analysisResults.size()]));
                this.treeViewerType = name;

            } else if (name.equals(RULE_TREE_VIEWER_TYPE)) {
                this.createRuleTreeViewer(this.parent);
                // We reinsert inputs from previous TreeViewer in the current
                // one
                this.getViewer().setInput(
                        this.analysisResults.toArray(new CheckResult[this.analysisResults.size()]));
                this.treeViewerType = name;

            }
            // This call is necessary to refresh the table in the parent
            // Composite.
            this.parent.layout();
        }

    }

}
