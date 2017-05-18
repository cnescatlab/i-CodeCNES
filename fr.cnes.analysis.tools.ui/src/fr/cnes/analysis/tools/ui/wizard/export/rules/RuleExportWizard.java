/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export.rules;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import fr.cnes.analysis.tools.export.Export;

/**
 * RuleExportWizard
 * 
 * @version 3.0
 * @since 2017-07-12
 * 
 *        This Wizard contains and handle the different Wizard Page to export
 *        analysis data of the Violations view. The finish performing of this
 *        Wizard must be done by a Wizard Page of type (or extending) the
 *        NewFileWizardPage class, that's also the reason a IStructuredSelection
 *        is necessary while calling the Wizard to be able to build the
 *        NewFileWizardPage classes.
 */
public class RuleExportWizard extends Wizard implements INewWizard {

    /** The main page containing the radio to choose the export's format. */
    private RuleExportWizardPage    mainPage;
    /** The class that will be used to export the file**/
    private RuleCreationFileExportWizardPage fileCreationPage;
    /** The selection of elements to build the NewFileWizardPage classes */
    private IStructuredSelection    selection;
    /** Exporter service */
	private Export exporter;
	
	/** Export Format chosen*/
	private String formatChosen =  "no export chosen";

    public String getFormatChosen() {
		return formatChosen;
	}

	public void setFormatChosen(String formatChosen) {
		this.formatChosen = formatChosen;
	}

	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
     * org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
    public void init(IWorkbench pWorkbench, IStructuredSelection pSelection) {
        this.selection = pSelection;
        this.exporter = new Export();
        /*
         * We force previous and next buttons as we don't use the default order
         * of page selection that is the one in which each page were added and
         * also because we willn't use all pages that we've added to the Wizard.
         */
        this.setForcePreviousAndNextButtons(true);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        final IFile file = ((WizardNewFileCreationPage) this.getContainer().getCurrentPage())
                .createNewFile();
        return (file != null);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        mainPage = new RuleExportWizardPage(selection, exporter);
        if(exporter.getAvailableFormats().size() > 0){
        	fileCreationPage = new RuleCreationFileExportWizardPage(selection, "unknown");
        }
        this.addPage(mainPage);
        this.addPage(fileCreationPage);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    @Override
    public boolean canFinish() {
        return this.getContainer().getCurrentPage().isPageComplete();
    }

}
