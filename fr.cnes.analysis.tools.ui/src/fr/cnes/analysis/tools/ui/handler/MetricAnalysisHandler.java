/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.MetricAnalysisJob;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.view.MetricsView;

/**
 * This class is the Handler that linked together action button on the menu and
 * analyze function located
 */
public class MetricAnalysisHandler extends AbstractAnalysisHandler {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(MetricAnalysisHandler.class.getName());

    /**
     * List of analyzed files
     */
    private final List<String> analyzedFiles = new ArrayList<String>();

    /**
     * Package/Explorer chosen for the analysis
     */
    private final IProject selectedProject = getActiveProject();

    /**
     * @return selectedProject class attribute
     */
    public IProject getSelectedProject() {
        return selectedProject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.handler.AbstractAnalysisHandler#runAnalysis
     * (java.util.List, java.lang.String)
     */
    @Override
    public void runAnalysis(final List<IPath> files, final String pAnalyzerID) {
        LOGGER.finest("Begin runAnalysis method");

        // Clear the analyzedFiles list in order to have the new analyzed files
        analyzedFiles.clear();

        // Instantiate analyzer
        final MetricAnalysisJob analysis = new MetricAnalysisJob(pAnalyzerID, files);

        // run analysis
        analysis.setUser(true);
        analysis.schedule();

        // add change listener to check when the job is done
        analysis.addJobChangeListener(new JobChangeAdapter() {

            @Override
            public void done(final IJobChangeEvent event) {
                Display.getDefault().asyncExec(new Runnable() {

                    @Override
                    public void run() {
                        // We generate an XML file only if the analysis wasn't
                        // interrupted
                        if (analysis.getResult().isOK()) {
                            MetricAnalysisHandler.this.updateView(analysis.getValues());
                        }
                    }
                });
            }
        });

        LOGGER.finest("End runAnalysis method");
    }

    /**
     * Update the view
     * 
     * @param values
     *            list of file value .
     */
    private void updateView(final List<FileValue> values) {
        LOGGER.finest("Begin updateView method");

        try {
            // get the page
            final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage();

            // open view
            page.showView(MetricsView.VIEW_ID);

            // get view
            final MetricsView view = (MetricsView) page.findView(MetricsView.VIEW_ID);

            // show rules analyze results
            if (view != null) {
                view.display(values, this.getSelectedProject(), this.getAuthor(), this.getDate());
                /* Add IMarkers everywhere there is a metric event */
                view.insertMarkers();
            }

        } catch (final PartInitException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final EmptyProviderException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End updateView method");
    }

    /**
     * @return The Eclipse user name that ran the analysis
     */
    private String getAuthor() {
        String author = System.getProperty("user.name");
        if (author.isEmpty()) {
            author = "Unknown";
        }
        return author;
    }

    /**
     * @return Date of the analysis
     */
    public String getDate() {
        final String format = "YYYY-MM-dd";
        final SimpleDateFormat formater = new SimpleDateFormat(format, Locale.FRANCE);
        final Date date = new Date();
        return formater.format(date);
    }

    /**
     * @return IProject Project selected in the active view
     */
    public IProject getActiveProject() {

        // Set the project null
        IProject project = null;

        // Get the selection
        final ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getActivePage().getActivePart().getSite().getSelectionProvider().getSelection();

        // Get the project of the element selected
        if (selection instanceof IStructuredSelection) {
            final Object element = ((IStructuredSelection) selection).getFirstElement();

            if (element instanceof IResource) {
                project = ((IResource) element).getProject();
            }
        }
        return project;
    }

}
