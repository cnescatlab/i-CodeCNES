/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.view.metrics.FunctionMetricDescriptor;
import fr.cnes.analysis.tools.ui.view.metrics.MetricContentProvider;
import fr.cnes.analysis.tools.ui.view.metrics.MetricLabelProvider;
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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * View displaying the metrics computation results.
 * 
 * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisView
 */
public class MetricsView extends AbstractAnalysisView {

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
                                    /*
                                     * We don't compare location if the location
                                     * is a file's one.
                                     */
                                    if ((value1.getLocation() == null
                                            || value1.getLocation().isEmpty())
                                            && (value2.getLocation() == null
                                                    || value2.getLocation().isEmpty())) {
                                        res = 0;
                                    } else if ((value1.getLocation() == null
                                            || value1.getLocation().isEmpty())
                                            && (value2.getLocation() != null
                                                    || value2.getLocation().isEmpty())) {
                                        return -1;
                                    } else if ((value1.getLocation() != null
                                            || value1.getLocation().isEmpty())
                                            && (value2.getLocation() == null
                                                    || value2.getLocation().isEmpty())) {
                                        return 1;
                                    } else {
                                        res = value1.getLocation().compareTo(value2.getLocation());
                                    }
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
        this.analysisResult.clear();
        this.getViewer().setInput(new CheckResult[0]);
        this.getViewer().refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.AbstractAnalysisView#fillView()
     */
    @Override
    protected void fillView() {
        // TODO not implemented yet

    }

    /**
     * @return the analysisResult
     */
    public final Set<CheckResult> getAnalysisResult() {
        return analysisResult;
    }

}
