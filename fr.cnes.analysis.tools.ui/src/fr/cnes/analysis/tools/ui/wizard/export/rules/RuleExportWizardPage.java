/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export.rules;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import fr.cnes.analysis.tools.export.Export;

/**
 * RuleExportWizardPage
 * 
 * @version 2.0
 * @since 2017-07-12
 * 
 *        This class page is the main one of the Rule Export Wizard. This class
 *        is requesting the user the export's format and defining the next page
 *        to call situationally.
 */
public class RuleExportWizardPage extends WizardPage {

	/**
	 * Exporter service in charge of the analysis 
	 */
	private Export exporter;
	
	/**
	 * Buttons list of all buttons offering available format for exportation.
	 */
	private List<Button> formatButtons;
    
	/**
     * Create the wizard.
	 * @param pSelection 
	 * @param exporter 
     */
    public RuleExportWizardPage(IStructuredSelection pSelection, Export exporter) {
        super("RuleExportWizardPage");
        this.setTitle("i-Code CNES - Rules violations export");
        this.setDescription("Description : Please choose the export format of your analyse.");
        this.exporter = exporter;
        formatButtons = new ArrayList<>();
    }
    
    public boolean canFlipToNextPage(){
    	return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.
     * widgets.Composite)
     */
    @Override
    public void createControl(Composite pParent) {
        final Composite container = new Composite(pParent, SWT.NULL);
        setControl(container);
        container.setLayout(new GridLayout(2, false));
        for(String export : exporter.getAvailableFormats().keySet()){
        	Button btn = new Button(container, SWT.RADIO);
            btn.setText(export);
            btn.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					RuleCreationFileExportWizardPage nextPage = (RuleCreationFileExportWizardPage) getWizard().getPage("RuleCreationFileExportWizardPage");
					nextPage.updateFormat(btn.getText());
				}
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            this.formatButtons.add(btn);
        }
        if(this.formatButtons.size()>0){
        	this.formatButtons.get(0).setSelection(true);
			RuleCreationFileExportWizardPage nextPage = (RuleCreationFileExportWizardPage) getWizard().getPage("RuleCreationFileExportWizardPage");
        	nextPage.updateFormat(this.formatButtons.get(0).getText());
        }
        
        
    }
    

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    @Override
    public IWizardPage getNextPage() {
        return this.getWizard().getPage("RuleCreationFileExportWizardPage");

    }
    @Override
    public boolean isPageComplete() {
        return false;
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getPreviousPage()
     */
    @Override
    public IWizardPage getPreviousPage() {
        return null;
    }

}
