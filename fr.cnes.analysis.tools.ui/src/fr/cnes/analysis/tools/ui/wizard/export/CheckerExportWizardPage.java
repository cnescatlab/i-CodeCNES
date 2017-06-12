/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export;

import fr.cnes.analysis.tools.export.Export;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * This class is the main page of the {@link CheckerExportWizard}. It's
 * responsible of suggesting available formats for export to the user using
 * {@link Export#getAvailableFormats()} and indicating the chosen format to the
 * next page {@link CheckerFileCreationExportWizardPage}.
 * 
 * @version 3.0
 * @since 2.0
 * 
 * 
 */
public class CheckerExportWizardPage extends WizardPage {

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
     * 
     * @param pSelection
     * @param exporter
     *            service.
     */
    public CheckerExportWizardPage(IStructuredSelection pSelection, Export exporter) {
        super("RuleExportWizardPage");
        this.setTitle("i-Code CNES - Analysis result export.");
        this.setDescription(
                "Description : Please choose the format of the export of you file. \nNote: This export will contain both metric & rules analysis result.");
        this.exporter = exporter;
        formatButtons = new ArrayList<>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
     */
    @Override
    public boolean canFlipToNextPage() {
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
        for (String export : exporter.getAvailableFormats().keySet()) {
            Button btn = new Button(container, SWT.RADIO);
            btn.setText(export);
            /*
             * A new listener is set to update format chosen in the next page.
             */
            btn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    CheckerFileCreationExportWizardPage nextPage = (CheckerFileCreationExportWizardPage) getWizard()
                            .getPage("RuleCreationFileExportWizardPage");
                    nextPage.updateFormat(btn.getText());
                }
            });
            this.formatButtons.add(btn);
        }
        if (this.formatButtons.size() > 0) {
            this.formatButtons.get(0).setSelection(true);
            CheckerFileCreationExportWizardPage nextPage = (CheckerFileCreationExportWizardPage) getWizard()
                    .getPage("RuleCreationFileExportWizardPage");
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
     */
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
