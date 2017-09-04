/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.Export;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;
import fr.cnes.analysis.tools.ui.view.MetricsView;
import fr.cnes.analysis.tools.ui.view.ViolationsView;

/**
 * This class is an adaptive {@link WizardNewFileCreationPage} which format is
 * pending the selection of the user in the previous page
 * {@link CheckerExportWizard}.
 * 
 * <p>
 * On {@link #getInitialContents()} this class realize an export using
 * {@link fr.cnes.analysis.tools.export.Export} service.
 * </p>
 * 
 * @since 3.0
 */
public class CheckerFileCreationExportWizardPage extends WizardNewFileCreationPage {
    /** The logger **/
    public static final Logger LOGGER = Logger
                    .getLogger(CheckerFileCreationExportWizardPage.class.getName());

    /** Export service used */
    private Export exporter;
    /** Export format requested by the user */
    private String requestedFormat;
    /** Parameters requested by the export plugin */
    private Map<String, String> parameters;

    /**
     * Constructor for this wizard page
     * 
     * @param selection
     *            the selection
     * @param pRequestedFormat
     *            the format requested by the user (can be default one also).
     */
    public CheckerFileCreationExportWizardPage(final IStructuredSelection selection,
                    String pRequestedFormat) {
        super("RuleCreationFileExportWizardPage", selection);
        exporter = new Export();
        requestedFormat = pRequestedFormat;
        this.setTitle("i-Code CNES - Rules export (" + pRequestedFormat + ")");
        this.setDescription("Description : Create a result export file in " + pRequestedFormat
                        + " format.");
        this.setFileExtension(exporter.getAvailableFormats().get(pRequestedFormat));
    }

    /**
     * This function update all field of the page and the file format pending
     * the {@code pRequestedFormat} value.
     * 
     * <p>
     * This class is currently called by the previous page
     * {@link CheckerExportWizardPage} every time an user select an export
     * format for his file.
     * </p>
     * 
     * @param pRequestedFormat
     *            to export.
     */
    public void updateFormat(String pRequestedFormat) {
        requestedFormat = pRequestedFormat;
        this.setTitle("i-Code CNES - Analysis results export (" + pRequestedFormat + ")");
        this.setDescription("Description : Create a result export file in " + pRequestedFormat
                        + " format.");
        this.setFileExtension(exporter.getAvailableFormats().get(pRequestedFormat));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.dialogs.WizardNewFileCreationPage#getInitialContents()
     */
    @Override
    public InputStream getInitialContents() {
        LOGGER.finest("Begin getInitialContents method");

        InputStream stream = null;
        try {
            // create a temporary file
            final File temp;
            temp = File.createTempFile("export",
                            "." + exporter.getAvailableFormats().get(this.requestedFormat));
            final Export export = new Export();
            // get the page
            final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage();

            // open view
            page.showView(ViolationsView.VIEW_ID);

            // get view
            final ViolationsView violationView = (ViolationsView) page
                            .findView(ViolationsView.VIEW_ID);
            page.showView(MetricsView.VIEW_ID);
            final MetricsView metricsView = (MetricsView) page.findView(MetricsView.VIEW_ID);
            /*
             * Retrieving violations to export into a list.
             */
            final List<CheckResult> checkResults = new ArrayList<>();
            checkResults.addAll(violationView.getAnalysisResults());
            checkResults.addAll(metricsView.getAnalysisResult());
            /* exporting the violations into the temp file */
            export.export(checkResults, temp, parameters);

            stream = new FileInputStream(temp);
        } catch (final IOException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        } catch (final PartInitException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        } catch (NoContributorMatchingException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        } catch (NoExtensionIndicatedException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        } catch (CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        LOGGER.finest("End getInitialContents method");
        return stream;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    @Override
    public IWizardPage getNextPage() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getPreviousPage()
     */
    @Override
    public IWizardPage getPreviousPage() {
        return this.getWizard().getPage("RuleExportWizardPage");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
     */
    @Override
    public boolean isPageComplete() {
        return this.validatePage();
    }

    /**
     * This function can be called from other pages to update parameters sent in
     * the export.
     * 
     * @param params
     *            the new parameter to set.
     */
    public void updateParameters(Map<String, String> params) {
        this.parameters = params;
    }
}
