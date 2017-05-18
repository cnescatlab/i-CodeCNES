package fr.cnes.analysis.tools.ui.wizard.export.rules;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.export.Export;
import fr.cnes.analysis.tools.export.exception.NoExportClassFoundInContributions;
import fr.cnes.analysis.tools.export.exception.NoIndicatedFormatInFilePathException;
import fr.cnes.analysis.tools.ui.view.ViolationsView;

public class RuleCreationFileExportWizardPage extends WizardNewFileCreationPage {
    /** The logger **/
    public static final Logger LOGGER = Logger.getLogger(RuleCreationFileExportWizardPage.class.getName());

    private Export exporter;
    private String requestedFormat;
    /**
     * Constructor for this wizard page
     * 
     * @param selection
     *            the selection
     */
    public RuleCreationFileExportWizardPage(final IStructuredSelection selection, String pRequestedFormat) {
        super("RuleCreationFileExportWizardPage", selection);
        exporter = new Export();
        requestedFormat = pRequestedFormat;
        this.setTitle("i-Code CNES - Rules export ("+pRequestedFormat+")");
        this.setDescription("Description : Create a result export file in "+pRequestedFormat+" format.");
        this.setFileExtension(exporter.getAvailableFormats().get(pRequestedFormat));
        
    }
    
    public void updateFormat(String pRequestedFormat){
    	  requestedFormat = pRequestedFormat;
          this.setTitle("i-Code CNES - Rules export ("+pRequestedFormat+")");
          this.setDescription("Description : Create a result export file in "+pRequestedFormat+" format.");
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
            File temp;
            temp = File.createTempFile("export", "."+exporter.getAvailableFormats().get(this.requestedFormat));
            Export export = new Export();
            // get the page
            final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage();

            // open view
            page.showView(ViolationsView.VIEW_ID);

            // get view
            final ViolationsView view = (ViolationsView) page.findView(ViolationsView.VIEW_ID);
            List<Violation> violations = new ArrayList<>();
            violations.addAll(view.getAnalysisResults());
            export.exportViolation(violations, temp);

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
        } catch (NoExportClassFoundInContributions e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoIndicatedFormatInFilePathException e) {
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
