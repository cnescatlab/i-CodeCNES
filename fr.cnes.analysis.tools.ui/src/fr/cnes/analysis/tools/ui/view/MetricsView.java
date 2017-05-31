/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.Export;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;
import fr.cnes.analysis.tools.ui.view.metrics.FileMetricDescriptor;
import fr.cnes.analysis.tools.ui.view.metrics.FunctionMetricDescriptor;
import fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor;
import fr.cnes.analysis.tools.ui.view.metrics.MetricContentProvider;
import fr.cnes.analysis.tools.ui.view.metrics.MetricDescriptor;
import fr.cnes.analysis.tools.ui.view.metrics.MetricLabelProvider;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * View displaying the metrics computation results.
 * 
 * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisView
 */
public class MetricsView extends AbstractExportableView {

    /**
     * Relative path to the XSD contained in the project for XML analysisResult
     * file validation.
     */
    public static final String XSD_FILEPATH = "resource/xsd/analysisResult.xsd";

    /** View ID. **/
    public static final String VIEW_ID = MetricsView.class.getName();

    /** Bounds value. **/
    public static final int[] BOUNDS = { 200, 75, 75, 75, 75, 200, 200 };

    /** Columns titles **/
    public static final String[] TITLES = { "Metric", "Total", "Mean", "Minimum", "Maximum" };

    /**
     * 
     */
    public static final List<IMarker> MARKERS = new ArrayList<IMarker>();

    /** Logger **/
    private static final Logger LOGGER = Logger.getLogger(MetricsView.class.getName());

    /**
     * This attribute store all the analysis results files.
     */
    private Set<CheckResult> analysisResult = new HashSet<CheckResult>();

    /** The project analyzed */
    private String analysisProject = "unknown";

    /** The name of the user who ran the analysis */
    private String author = "unknown";

    /** The moment of the analysis */
    private String date = "unknown";

    /**
     * Empty constructor.
     */
    public MetricsView() {
        super(BOUNDS, TITLES);
        analysisResult = new TreeSet<>(new Comparator<CheckResult>() {

            @Override
            public int compare(final CheckResult value1, final CheckResult value2) {

                int res = value1.getId().compareTo(value2.getId());
                if (res == 0) {
                    res = value1.getFile().getAbsolutePath()
                            .compareTo(value2.getFile().getAbsolutePath());
                }
                return res;
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisView#createColumns()
     */
    @Override
    protected void createColumns() {
        LOGGER.finest("Begin createColumns method");

        this.getViewer().setContentProvider(new MetricContentProvider());
        TreeViewerColumn col;
        for (int i = 0; i < this.getTitles().length; i++) {
            // Create the column
            col = this.createTreeViewerColumn(this.getTitles()[i], this.getBounds()[i]);

            // Add a label provider
            col.setLabelProvider(new MetricLabelProvider(i));
        }

        LOGGER.finest("End createColumns method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisView#
     * addDoubleClickAction ()
     */

    @Override
    protected void addDoubleClickAction() {
        LOGGER.finest("begin method addDoubleClickAction");
        final TreeViewer viewer = this.getViewer();

        viewer.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                final TreeViewer tViewer = (TreeViewer) event.getViewer();
                final IStructuredSelection thisSelection = (IStructuredSelection) event
                        .getSelection();
                final Object selectedNode = thisSelection.getFirstElement();

                tViewer.setExpandedState(selectedNode, !tViewer.getExpandedState(selectedNode));
                // if it is a leaf -> open the file
                if (!tViewer.isExpandable(selectedNode)
                        && selectedNode instanceof FunctionMetricDescriptor) {

                    // get Path of the file & Line of the
                    // File containing the
                    // Metric
                    final IPath path = ((FunctionMetricDescriptor) selectedNode).getFilePath();
                    final int line = ((FunctionMetricDescriptor) selectedNode).getLine();

                    // get resource
                    final IFile fileToOpen = ResourcesPlugin.getWorkspace().getRoot()
                            .getFileForLocation(path);
                    final IResource res = fileToOpen;

                    // open file in editor
                    MetricsView.this.openFileInEditor(res, line);

                }
            }

        });

        LOGGER.finest("end method addDoubleClickAction");
    }

    /**
     * Procedure insertMarkers
     * 
     * Clean every marker in a file of the type violation error and then add the
     * ones violating metric limits.
     * 
     */
    public void insertMarkers() {
        LOGGER.finest("begin method insertMarkers");
        /*
         * To avoid to clean two times markers in the same document, we use an
         * HashSet to refer cleaned files.
         */
        final HashSet<IFile> cleanedFiles = new HashSet<IFile>();
        String metricName;
        Float value, limit;
        IFile file;
        try {
            for (final MetricDescriptor metricD : ((MetricContentProvider) this.getViewer()
                    .getContentProvider()).getConverter().getContainer()) {
                metricName = metricD.getName();
                for (final FileMetricDescriptor fileMetricD : metricD.getDescriptors()) {
                    for (final FunctionMetricDescriptor funMetric : fileMetricD.getDescriptors()) {
                        if (!funMetric.hasRightValue()) {
                            // Has we are going to
                            // add a new marker, we
                            // verify
                            // that
                            // the file was clean
                            file = ResourcesPlugin.getWorkspace().getRoot()
                                    .getFileForLocation(funMetric.getFilePath());
                            value = funMetric.getValue();
                            limit = PlatformUI.getPreferenceStore()
                                    .getFloat(funMetric.getMetricId() + PreferencesUIUtils.VALUE
                                            + PreferencesUIUtils.LEVELS[PlatformUI
                                                    .getPreferenceStore()
                                                    .getInt(PreferencesUIUtils.LEVEL)]);
                            if (!cleanedFiles.contains(file)) {
                                // if it's not,
                                // then we clean
                                // markers in it
                                cleanedFiles.add(file);
                                file.deleteMarkers(
                                        "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker",
                                        true, 1);
                            }
                            ViolationErrorMarker.createMarker(file, funMetric.getLine(),
                                    funMetric.getName(), metricName + " | Value is " + value
                                            + " while limit was set to " + limit);
                        }
                    }

                }
            }
            // One time all markers have been insert, we refresh all
            // decorators.
            final IDecoratorManager manager = PlatformUI.getWorkbench().getDecoratorManager();

            manager.update("fr.cnes.analysis.tools.ui.decorators.violationwarningdecorator");
            manager.update("fr.cnes.analysis.tools.ui.decorators.violationerrordecorator");
        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Marker problem", exception.getMessage());
        }

        LOGGER.finest("end method insertMarkers");

    }

    /**
     * Open the file containing the metric at the metric's function line.
     * 
     * @param res
     *            the file to open
     * @param line
     *            the line where the file should open
     */
    private void openFileInEditor(final IResource res, final int line) {
        LOGGER.finest("begin method openFileInEditor");
        final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getActivePage();
        try {
            // Before creating the marker
            res.deleteMarkers(IMarker.MARKER, false, 1);
            final IMarker marker = res.createMarker(IMarker.MARKER);
            marker.setAttribute(IMarker.LINE_NUMBER, line);
            marker.setAttribute("Class", "Metric");

            IDE.openEditor(page, marker);
        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Marker problem", exception.getMessage());
        }
        LOGGER.finest("end method openFileInEditor");
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.ExportableView#toCSV()
     */
    @Override
    protected void toCSV() throws IOException {
        LOGGER.finest("Begin toCSV method");

        getOut().write("Metric, File, Location, Total, "
                + "Mean, Minimum, Maximum, Resource Causing Minimum , Resource Causing Maximum\n");

        for (final MetricDescriptor value : ((MetricContentProvider) this.getViewer()
                .getContentProvider()).getConverter().getContainer()) {
            this.getOut().write(value.getName() + ",,," + this.getCSVText(value));
            for (final FileMetricDescriptor fValue : value.getDescriptors()) {
                this.getOut().write(
                        value.getName() + "," + fValue.getName() + ",," + this.getCSVText(fValue));
                if (!fValue.getDescriptors().isEmpty()) {
                    for (final FunctionMetricDescriptor funcValue : fValue.getDescriptors()) {
                        this.getOut().write(value.getName() + "," + fValue.getName() + ","
                                + funcValue.getName() + "," + this.getCSVText(funcValue));
                    }
                }
            }
        }

        LOGGER.finest("End toCSV method");
    }

    /**
     * This method creates the common text for any Viewable to put in the CSV
     * file.
     * 
     * @param element
     *            the Viewable element to get text
     * @return CSV formatted text of this element
     */
    private String getCSVText(final IMetricDescriptor element) {
        LOGGER.finest("Begin getText method");

        final String text = this.convertToString(element.getValue()) + ","
                + this.convertToString(element.getMean()) + ","
                + this.convertToString(element.getMinimum()) + ","
                + this.convertToString(element.getMaximum()) + "," + element.getMinCause() + ","
                + element.getMaxCause() + "\n";

        LOGGER.finest("End getText method");
        return text;
    }

    /**
     * Method used to convert float value into String value for CSV export. If
     * the float value is infinite or NaN, the text returned is "--". Otherwise,
     * it returns Float.toString() value.
     * 
     * @param value
     *            float value to convert into String
     * @return toString() value, "--" if the value is infinite or NaN
     */
    private String convertToString(final Float value) {
        String text;
        if (value.isInfinite() || value.isNaN()) {
            text = "--";
        } else {
            text = value.toString();
        }
        return text;
    }

    /**
     * Displays the analyze results on the view.
     * 
     * @param values
     *            the descriptors to show on the view
     * @param pProject
     *            The project selected to run the analysis
     * @param pAnalysisDate
     *            The date of the analysis
     * @param pAnalysisAuthor
     *            The user who ran the analysis
     * @return
     * @throws EmptyProviderException
     *             when source provider to determine view type is not found (not
     *             necessarily used)
     */
    public void display(final List<CheckResult> values) throws EmptyProviderException {
        LOGGER.finest("Begin display method");

        synchronized (this) {
            final Set<CheckResult> listInputs = new TreeSet<CheckResult>(
                    new Comparator<CheckResult>() {

                        @Override
                        public int compare(final CheckResult value1, final CheckResult value2) {

                            int res = 0;
                            res = value1.getId().compareTo(value2.getId());
                            if (res == 0) {
                                res = value1.getFile().getAbsolutePath()
                                        .compareTo(value2.getFile().getAbsolutePath());
                                if (res == 0) {
                                    res = value1.getLocation().compareTo(value2.getLocation());
                                }
                            }
                            return res;
                        }
                    });

            if (this.getViewer().getInput() != null) {
                for (final CheckResult input : (CheckResult[]) this.getViewer().getInput()) {
                    listInputs.add(input);
                }
            }

            for (final CheckResult value : values) {
                listInputs.add(value);
            }
            analysisResult = listInputs;
            this.getViewer().setInput(listInputs.toArray(new CheckResult[listInputs.size()]));
        }

        this.getViewer().refresh();
        LOGGER.finest("End display method");
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

    @Override
    protected void fillView() {
        // TODO not implemented yet

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.AbstractExportableView#toXML(java.io.File)
     */
    @Override
    protected void toXML(final File file) throws IOException {
        LOGGER.finest("begin method toXML");
        Export exportService = new Export();
        try {
            exportService.export(new ArrayList<>(this.analysisResult), file);
        } catch (NoContributorMatchingException | NoExtensionIndicatedException e) {
            e.printStackTrace();
        }
        LOGGER.finest("end method toXML");

    }

}
