/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.export.ExportService;

/**
 * This Wizard contains and handle the different Wizard Page to export analysis
 * data of the Violations view. When {@link #performFinish()} is called, the
 * {@link CheckerFileCreationExportWizardPage} export the result in the format
 * chosen by the user.
 *
 * <p>
 * Available formats are defined by the
 * {@link fr.cnes.analysis.tools.export.ExportService} service using
 * {@link ExportService#getAvailableFormats()}.
 * </p>
 *
 * <p>
 * To add a new format to export, it's necessary to contribute to the
 * {@link ExportService} service.
 * </p>
 *
 * @version 3.0
 * @since 2.0
 */
public class CheckerExportWizard extends Wizard implements IExportWizard, INewWizard {

    /**
     * Class name
     */
    private static final String CLASS = CheckerExportWizard.class.getName();
    /**
     * The main page containing the radio to choose the export's format.
     */
    private CheckerExportWizardPage mainPage;
    /**
     * The class that will be used to export the file
     **/
    private CheckerFileCreationExportWizardPage fileCreationPage;
    /**
     * The selection of elements to build the NewFileWizardPage classes
     */
    private IStructuredSelection selection;
    /**
     * Exporter service
     */
    private ExportService exporter;

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
     * org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
    public void init(IWorkbench pWorkbench, IStructuredSelection pSelection) {
        final String method = "init";
        ICodeLogger.entering(CLASS, method, new Object[]{
                pWorkbench, pSelection
        });
        this.selection = pSelection;
        this.exporter = new ExportService();
        /*
         * We force previous and next buttons as we don't use the default order
         * of page selection that is the one in which each page were added and
         * also because we willn't use all pages that we've added to the Wizard.
         */
        this.setForcePreviousAndNextButtons(true);

        ICodeLogger.exiting(CLASS, method);

    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        final String method = "performFinish";
        ICodeLogger.entering(CLASS, method);
        final IFile file = ((WizardNewFileCreationPage) this.getContainer().getCurrentPage())
                .createNewFile();
        final boolean performFinish = (file != null);
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(performFinish));
        return performFinish;

    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        final String method = "addPages";
        ICodeLogger.entering(CLASS, method);
        mainPage = new CheckerExportWizardPage(selection, exporter);
        if (exporter.getAvailableFormats().size() > 0) {
            fileCreationPage = new CheckerFileCreationExportWizardPage(selection, "unknown");
        }
        this.addPage(mainPage);
        this.addPage(fileCreationPage);
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    @Override
    public boolean canFinish() {
        final String method = "canFinish";
        ICodeLogger.entering(CLASS, method);
        final boolean canFinish = this.getContainer().getCurrentPage().isPageComplete();
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(canFinish));
        return canFinish;
    }

}
