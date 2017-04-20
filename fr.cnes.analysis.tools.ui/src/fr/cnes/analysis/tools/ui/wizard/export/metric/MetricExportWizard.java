package fr.cnes.analysis.tools.ui.wizard.export.metric;

import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.analysis.tools.ui.wizard.export.metric.csv.MetricCSVExportWizardPage;
import fr.cnes.analysis.tools.ui.wizard.export.metric.xml.MetricXMLExportWizardPage;

/**
 * RuleExportWizard
 * 
 * @version 2.0
 * @since 2017-07-12
 * 
 *        This Wizard contains and handle the different Wizard Page to export
 *        analysis data of the MetricsView class. The finish performing of this
 *        Wizard must be done by a Wizard Page of type (or extending) the
 *        NewFileWizardPage class, that's also the reason a IStructuredSelection
 *        is necessary while calling the Wizard to be able to build the
 *        NewFileWizardPage classes.
 */
public class MetricExportWizard extends Wizard implements INewWizard {

    /** The logger */
    public static final Logger LOGGER = Logger.getLogger(MetricExportWizard.class.getName());

    /** The main page in which is made the export format decision by the user */
    private MetricExportWizardPage    mainPage;
    /** The CSV export page */
    private MetricCSVExportWizardPage exportCSV;
    /** The XML export page */
    private MetricXMLExportWizardPage exportXML;
    /** The selection of elements to build the NewFileWizardPage classes */
    private IStructuredSelection      selection;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
     * org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
    public void init(IWorkbench workbench, IStructuredSelection pSelection) {
        LOGGER.finest("begin method init");
        this.selection = pSelection;
        this.setForcePreviousAndNextButtons(true);
        LOGGER.finest("end of method init");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        LOGGER.finest("begin method performFinish");
        final IFile file = ((WizardNewFileCreationPage) this.getContainer().getCurrentPage())
                .createNewFile();

        LOGGER.finest("end method performFinish");
        return (file != null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        LOGGER.finest("begin method addPages");
        mainPage = new MetricExportWizardPage();
        this.addPage(mainPage);
        exportCSV = new MetricCSVExportWizardPage(selection);
        this.addPage(exportCSV);
        exportXML = new MetricXMLExportWizardPage(selection);
        this.addPage(exportXML);

        LOGGER.finest("end method addPages");

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    @Override
    public boolean canFinish() {
        LOGGER.finest("begin method canFinish");
        boolean finish;
        if (this.getContainer().getCurrentPage() == mainPage) {
            finish = false;
        } else {
            finish = this.getContainer().getCurrentPage().isPageComplete();
        }
        LOGGER.finest("end method canFinish");
        return finish;
    }

}
