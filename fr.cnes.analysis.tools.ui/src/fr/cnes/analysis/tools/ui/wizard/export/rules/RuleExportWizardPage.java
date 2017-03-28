/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export.rules;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

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

    /** The radio button to choose CSV export format */
    private Button btnCsv;
    /** The radio button to choose XML export format */
    private Button btnXml;

    /**
     * Create the wizard.
     */
    public RuleExportWizardPage() {
        super("RuleExportWizardPage");
        setTitle("i-Code CNES - Rules violations export");
        setDescription("Description : Choosing the type of result's export.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.
     * widgets.Composite)
     */
    @Override
    public void createControl(Composite parent) {
        final Composite container = new Composite(parent, SWT.NULL);

        setControl(container);
        container.setLayout(new GridLayout(2, false));

        btnCsv = new Button(container, SWT.RADIO);
        btnCsv.setText("CSV");

        btnXml = new Button(container, SWT.RADIO);
        btnXml.setText("XML");

    }

    /**
     * @return the btnCsv
     */
    public Button getBtnCsv() {
        return btnCsv;
    }

    /**
     * @return the btnXml
     */
    public Button getBtnXml() {
        return btnXml;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    @Override
    public IWizardPage getNextPage() {
        IWizardPage nextPage;
        if (btnCsv.getSelection()) {
            nextPage = this.getWizard().getPage("RuleCSVExportWizardPage");
        } else {
            nextPage = this.getWizard().getPage("RuleXMLExportWizardPage");
        }
        return nextPage;

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
