/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export.rules.csv;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.analysis.tools.ui.view.ViolationsView;

/**
 * RuleCSVExportWizardPage
 * 
 * @version 2.0
 * @since 2017-07-12
 * 
 *        This class contain the Wizard Page that is being called by the
 *        RuleExportWizard when an user is exporting analysis results of the
 *        Violation's view in the format CSV.
 */
public class RuleCSVExportWizardPage extends WizardNewFileCreationPage {
    /** The logger **/
    public static final Logger LOGGER = Logger.getLogger(RuleCSVExportWizardPage.class.getName());

    /**
     * Constructor for this wizard page
     * 
     * @param selection
     *            the selection
     */
    public RuleCSVExportWizardPage(final IStructuredSelection selection) {
        super("RuleCSVExportWizardPage", selection);
        this.setTitle("i-Code CNES - Rules export (CSV)");
        this.setDescription("Description : Create a result export file in CSV format.");
        this.setFileExtension("csv");
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
            File temp;
            temp = File.createTempFile("export", ".tmp");

            // get the page
            final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage();

            // open view
            page.showView(ViolationsView.VIEW_ID);

            // get view
            final ViolationsView view = (ViolationsView) page.findView(ViolationsView.VIEW_ID);
            if (view != null) {
                view.exportToCSV(temp);
            }

            stream = new FileInputStream(temp);
        } catch (final IOException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final PartInitException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
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

}
